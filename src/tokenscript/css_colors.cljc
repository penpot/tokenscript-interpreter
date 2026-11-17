;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns tokenscript.css-colors
  "Native implementations of the bundled CSS color functions `rgb`,
  `rgba`, `hsl` and `hsla`.

  The scripts of the bundled schemas (`functions/css-rgb/rgb.tokenscript`
  and `functions/css-hsl/hsl.tokenscript`) define these functions. The
  native versions compute the same values with the same arithmetic, for
  the arguments the scripts accept: numbers, percentages, `deg` hues and
  a color as first argument. For any other argument they return nil and
  the interpreter runs the script, so errors are the script's. See
  `interpreter/native-function`."
  (:require
   [tokenscript.colors :as colors]
   [tokenscript.registry :as reg]
   [tokenscript.symbols :as sym]
   [tokenscript.util :as u]))

;; Arguments ------------------------------------------------------------------

(defn- nan?
  [v]
  #?(:clj (Double/isNaN (double v))
     :cljs (js/isNaN v)))

(defn- number-value
  "Value of a number symbol, nil for anything else, a null number or NaN."
  [s]
  (when (sym/number? s)
    (let [v (:value s)]
      (when (and (number? v) (not (nan? v)))
        v))))

(defn- unit-value
  "Value of a number symbol with the unit `unit`, as `x.to_number()`
  returns it, or nil."
  [s unit]
  (when (and (sym/number-with-unit? s) (= unit (:unit s)))
    (let [v (:value s)]
      (when (and (number? v) (not (nan? v)))
        (double v)))))

(defn- arguments
  "The argument list of a call, or the items of its only argument when it
  is a list (`rgb(255 0 0)`). Nil when the list is empty."
  [args]
  (let [parts (if (and (= 1 (count args)) (sym/list? (nth args 0)))
                (sym/value (nth args 0))
                args)]
    (when (seq parts)
      parts)))

(defn- clamp
  "`min(max(v, lo), hi)`."
  [v lo hi]
  (double (min (max v lo) hi)))

(defn- alpha-value
  "The alpha at `index` of `parts` as `[present? alpha]`: a number or a
  percentage, clamped to 0-1, nil when it is 1. Nil when the argument is
  neither."
  [parts index]
  (if (<= (count parts) index)
    [false nil]
    (let [part (nth parts index)]
      (when-some [a (if-some [percent (unit-value part "%")]
                      (/ percent 100.0)
                      (number-value part))]
        (let [alpha (clamp a 0.0 1.0)]
          [true (when (< alpha 1.0) alpha)])))))

(defn- with-alpha
  "`color` with the alpha argument applied, as the scripts do."
  [color [present? alpha]]
  (if present?
    (assoc color :alpha alpha)
    color))

;; Color spaces -----------------------------------------------------------------

(defn- number-channels?*
  [registry sub-type channels]
  (boolean
   (when-let [spec (reg/spec-by-name registry sub-type)]
     (every? (fn [channel]
               (u/name= "number" (get-in spec [:schema :properties channel :type])))
             channels))))

;; The answer for the last color registry seen, per color space.
(defonce ^:private number-channels-cache (atom {}))

(defn- number-channels?
  "Whether the color space `sub-type` is registered with number
  channels `channels`, as the scripts need to declare and fill it."
  [config sub-type channels]
  (let [registry (:colors config)
        [cached-registry answer] (get @number-channels-cache sub-type)]
    (if (identical? cached-registry registry)
      answer
      (let [answer (number-channels?* registry sub-type channels)]
        (swap! number-channels-cache assoc sub-type [registry answer])
        answer))))

(defn- convert
  "`color.to.<color-type>()`, nil when the color space has no initializer
  to be called as a method."
  [config to-type color color-type]
  (when (colors/initializer (:colors config) color-type)
    (to-type color color-type)))

;; Functions --------------------------------------------------------------------

(defn- rgb-channel
  "A channel of rgb(): a number 0-255 or a percentage, clamped and
  rounded. Nil for other arguments."
  [part]
  (when-some [v (if-some [percent (unit-value part "%")]
                  (/ (* percent 255.0) 100.0)
                  (number-value part))]
    (sym/number (u/js-round (clamp v 0.0 255.0)))))

(defn rgb
  "Native `rgb()` and `rgba()`, see functions/css-rgb/rgb.tokenscript.
  `to-type` converts a color to a color space by name. Nil when the
  arguments need the script."
  [config args to-type]
  (when (number-channels? config "Rgb" [:r :g :b])
    (when-let [parts (arguments args)]
      (let [first-part (nth parts 0)]
        (if (sym/color? first-part)
          (when-let [alpha (alpha-value parts 1)]
            (when-let [color (convert config to-type first-part "rgb")]
              (with-alpha color alpha)))
          (when (<= 3 (count parts))
            (let [r (rgb-channel (nth parts 0))
                  g (rgb-channel (nth parts 1))
                  b (rgb-channel (nth parts 2))
                  alpha (alpha-value parts 3)]
              (when (and r g b alpha)
                (with-alpha (sym/->ColorSymbol {"r" r "g" g "b" b} "Rgb" nil) alpha)))))))))

(defn- fraction
  "Saturation or lightness of hsl() as `[percent? value]`: a percentage
  divided by 100 or a plain number. Nil for other arguments."
  [part]
  (if-some [percent (unit-value part "%")]
    [true (/ percent 100.0)]
    (when-some [v (number-value part)]
      [false v])))

(defn hsl
  "Native `hsl()` and `hsla()`, see functions/css-hsl/hsl.tokenscript.
  `to-type` converts a color to a color space by name. Nil when the
  arguments need the script."
  [config args to-type]
  (when (number-channels? config "HSL" [:h :s :l])
    (when-let [parts (arguments args)]
      (let [first-part (nth parts 0)]
        (if (sym/color? first-part)
          (when-let [alpha (alpha-value parts 1)]
            (when-let [color (convert config to-type first-part "hsl")]
              (with-alpha color alpha)))
          (when (<= 3 (count parts))
            (let [hue (or (unit-value first-part "deg") (number-value first-part))
                  [s-percent? s] (fraction (nth parts 1))
                  [l-percent? l] (fraction (nth parts 2))
                  alpha (alpha-value parts 3)]
              (when (and hue s l alpha)
                ;; Plain numbers above 1 mean the CSS percentage scale
                (let [css-scale? (or (and (not s-percent?) (> s 1))
                                     (and (not l-percent?) (> l 1)))
                      s (if (and css-scale? (not s-percent?)) (/ s 100.0) s)
                      l (if (and css-scale? (not l-percent?)) (/ l 100.0) l)
                      h (sym/number (rem (+ (rem hue 360.0) 360.0) 360.0))]
                  (with-alpha (sym/->ColorSymbol {"h" h
                                                  "s" (sym/number (clamp s 0.0 1.0))
                                                  "l" (sym/number (clamp l 0.0 1.0))}
                                                 "HSL"
                                                 nil)
                    alpha))))))))))
