;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns tokenscript.importers.css
  "Imports CSS values as tokenscript: reads the CSS (or tokenscript) text
  of a linear gradient and writes it the way the `linear-gradient()`
  function of the bundled schemas takes it, or as the value of a
  `gradient` token.

  The text is tokenized and parsed, not matched: colors, calls and
  token references nest as they do in CSS. What tokenscript does not
  read the CSS way is rewritten: the direction becomes an angle in
  degrees, the CSS color functions get commas and their slash alpha
  syntax (`rgb(255 0 0 / 50%)`, which tokenscript reads as a division)
  becomes a fourth channel of the `rgba` or `hsla` of CSS, and stops
  without a position get the one CSS gives them. Token references stay.

  See docs/importers.md."
  (:require
   [clojure.string :as str]
   [tokenscript.importers.syntax :refer [code-at hex-digit? parse-value punctuation? tokenize]]
   [tokenscript.util :as u]))

;; Declarations ---------------------------------------------------------------

(defn- statements
  "The tokens of each statement of `tokens`, which a `;` ends."
  [tokens]
  (->> (partition-by #(punctuation? % ";") tokens)
       (remove #(punctuation? (first %) ";"))
       (map vec)))

(defn- declaration-start
  "The index the value of a statement starts at: after `background:` or
  `background-image:` when it is a CSS declaration."
  [tokens]
  (let [[property colon] tokens]
    (if (and (= :name (:type property))
             (contains? #{"background" "background-image"} (str/lower-case (:text property)))
             (some? colon)
             (punctuation? colon ":"))
      2
      0)))

(defn- parse-statement
  "The call a statement is: alone, or as the value of a `background`
  declaration. Nil when it is anything else."
  [text tokens]
  (when-let [[value index] (parse-value text tokens (declaration-start tokens))]
    (when (and (= :call (:type value))
               (= index (count tokens)))
      value)))

(defn- parse-call
  "The first call named `call-name` in `text`, which is a call or CSS. The
  declarations that are not such a call are left out, like the fallback
  color and the vendor prefixed lines CSS tools write. Nil when there is
  none."
  [text call-name]
  (when-let [tokens (tokenize text)]
    (->> (statements tokens)
         (keep #(parse-statement text %))
         (filter #(= call-name (:name %)))
         (first))))

;; Linear gradients -----------------------------------------------------------

(def ^:private default-angle 180)

(def ^:private side-angles
  {#{"top"}             0
   #{"top" "right"}    45
   #{"right"}           90
   #{"bottom" "right"} 135
   #{"bottom"}          180
   #{"bottom" "left"}  225
   #{"left"}            270
   #{"top" "left"}     315})

(def ^:private degrees-per-unit
  {"deg"  1
   "grad" 0.9
   "turn" 360
   "rad"  (/ 180 #?(:clj Math/PI :cljs js/Math.PI))})

(def ^:private color-functions
  "The CSS color functions the bundled schemas take, with their name
  for the three channels and for the three channels and an alpha."
  {"rgb"  ["rgb" "rgba"]
   "rgba" ["rgb" "rgba"]
   "hsl"  ["hsl" "hsla"]
   "hsla" ["hsl" "hsla"]})

(defn- round-2
  [number]
  (/ (#?(:clj Math/round :cljs js/Math.round) (* (double number) 100.0)) 100.0))

(defn- format-number
  [number]
  (u/num->str (round-2 number)))

(defn- name-of
  [value]
  (when (= :name (:type value))
    (str/lower-case (:text value))))

(defn- direction->angle
  "The angle in degrees, as a number, of the first argument of a gradient
  when it is its direction: `to right`, `0.25turn`. Nil when it is a
  color stop."
  [[value & more]]
  (cond
    (and (= "to" (name-of value)) (<= 1 (count more) 2) (every? name-of more))
    (get side-angles (into #{} (map name-of) more))

    (and (empty? more)
         (= :number (:type value))
         (contains? degrees-per-unit (:unit value)))
    (* (:value value) (get degrees-per-unit (:unit value)))))

(defn- hex-color?
  [text]
  (and (contains? #{4 5 7 9} (count text))
       (every? hex-digit? (map #(code-at text %) (range 1 (count text))))))

(defn- color->text
  "A color the way tokenscript takes it. A call of the CSS color functions
  gets commas, its slash before the alpha goes, and it is the `rgba` or
  `hsla` of CSS when it has an alpha. Other calls and references stay as
  written; nil for anything that is no color."
  [{:keys [type text name args]}]
  (case type
    :hash      (when (hex-color? text) text)
    :name      text
    :reference text
    :call      (if-let [[opaque translucent] (get color-functions name)]
                 (let [values (->> (apply concat args)
                                   (remove #(punctuation? % "/"))
                                   (map :text))]
                   (str (if (or (> (count values) 3) (= name translucent)) translucent opaque)
                        "(" (str/join ", " values) ")"))
                 text)
    nil))

(defn- position-percent
  "The percentage of a position, `0` standing for `0%`."
  [{:keys [type value unit]}]
  (when (= :number type)
    (cond
      (= "%" unit)                   value
      (and (nil? unit) (zero? value)) 0)))

(defn- position-of
  "A stop position: a percentage, or a reference to one."
  [value]
  (or (position-percent value)
      (when (= :reference (:type value))
        (:text value))))

(defn- arg->stops
  "The color stops an argument stands for: one, two when it has two
  positions, none for a color hint (a lone percentage). Nil when it is
  none of these. A stop without a position has a nil one."
  [[color & positions :as arg]]
  (cond
    (empty? arg)
    nil

    (and (empty? positions) (some? (position-percent color)))
    []

    :else
    (when-let [color (color->text color)]
      (let [positions (mapv position-of positions)]
        (when (and (<= (count positions) 2) (every? some? positions))
          (if (empty? positions)
            [{:color color :position nil}]
            (mapv (fn [position] {:color color :position position}) positions)))))))

(defn- spread-positions
  "Gives the stops without a position one, as CSS does: the first stop
  starts at 0%, the last one ends at 100% and the ones in between are
  spread evenly between the stops around them that have one."
  [stops]
  (let [last-index (dec (count stops))
        stops      (-> stops
                       (update-in [0 :position] #(if (nil? %) 0 %))
                       (update-in [last-index :position] #(if (nil? %) 100 %)))
        anchors    (into []
                         (keep-indexed (fn [index {:keys [position]}]
                                         (when (number? position)
                                           [index position])))
                         stops)]
    (into []
          (map-indexed
           (fn [index stop]
             (if (some? (:position stop))
               stop
               (let [[from-index from] (or (last (filter #(< (first %) index) anchors)) [0 0])
                     [to-index to]     (or (first (filter #(> (first %) index) anchors)) [last-index 100])
                     ratio             (/ (- index from-index) (- to-index from-index))]
                 (assoc stop :position (+ from (* (- to from) ratio)))))))
          stops)))

(defn- format-position
  "A position outside of the gradient moves to its end."
  [position]
  (if (number? position)
    (str (format-number (max 0 (min 100 position))) "%")
    position))

(defn linear-gradient
  "Reads a CSS `linear-gradient()` (or the tokenscript one), also as a
  `background` declaration (or several, as CSS tools copy them), as the
  value of a `gradient` token: a map
  with its `\"kind\"`, its `\"angle\"` in degrees and its `\"stops\"`, each
  with a `\"color\"` and a `\"position\"` in percent. See the namespace
  for what is rewritten. Returns nil for anything else, like a radial or
  a repeating gradient or a stop positioned in pixels."
  [text]
  (when (string? text)
    (when-let [{:keys [args]} (parse-call text "linear-gradient")]
      (let [angle     (direction->angle (first args))
            stop-args (cond->> args (some? angle) (rest))
            stops     (map arg->stops stop-args)]
        (when (every? some? stops)
          (let [stops (into [] cat stops)]
            (when (>= (count stops) 2)
              {"kind"  "linear"
               "angle" (format-number (or angle default-angle))
               "stops" (mapv (fn [{:keys [color position]}]
                               {"color"    color
                                "position" (format-position position)})
                             (spread-positions stops))})))))))

(defn gradient->tokenscript
  "Writes the value of a `gradient` token that has an angle and stops as
  a tokenscript `linear-gradient()`."
  [gradient]
  (let [stops (map (fn [stop]
                     (str (get stop "color") " " (get stop "position")))
                   (get gradient "stops"))]
    (str "linear-gradient("
         (str/join ", " (cons (str (get gradient "angle") "deg") stops))
         ")")))

(defn linear-gradient->tokenscript
  "The tokenscript `linear-gradient()` for a CSS one, nil when `text` is
  not a linear gradient."
  [text]
  (some-> (linear-gradient text)
          (gradient->tokenscript)))
