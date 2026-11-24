;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns tokenscript.importers.css
  "Imports CSS values as tokenscript. It reads the CSS or tokenscript
  text of a linear gradient. It writes the gradient in the form that the
  `linear-gradient()` function of the bundled schemas takes, or as the
  value of a `gradient` token.

  The importer tokenizes and parses the text. It does not match the text
  with patterns. So colors, calls and token references nest as they do
  in CSS.

  The importer rewrites the CSS that tokenscript does not read as CSS
  does:

  - The direction becomes an angle in degrees.
  - The CSS color functions get commas.
  - Their slash alpha syntax becomes a fourth channel of the `rgba` or
    `hsla` of CSS. Tokenscript reads `rgb(255 0 0 / 50%)` as a division.
  - A stop without a position gets the position that CSS gives it.

  Token references stay as they are.

  See docs/importers.md."
  (:require
   [clojure.string :as str]
   [tokenscript.importers.syntax :refer [code-at hex-digit? parse-value punctuation? tokenize]]
   [tokenscript.util :as u]))

;; Declarations ---------------------------------------------------------------

(defn- statements
  "Splits `tokens` into statements. A `;` ends each statement."
  [tokens]
  (->> (partition-by #(punctuation? % ";") tokens)
       (remove #(punctuation? (first %) ";"))
       (map vec)))

(defn- declaration-start
  "Finds the index where the value of a statement starts. In a CSS
  declaration, the value starts after `background:` or
  `background-image:`."
  [tokens]
  (let [[property colon] tokens]
    (if (and (= :name (:type property))
             (contains? #{"background" "background-image"} (str/lower-case (:text property)))
             (some? colon)
             (punctuation? colon ":"))
      2
      0)))

(defn- parse-statement
  "Parses the call in a statement. The call is the whole statement, or
  the value of a `background` declaration. Gives nil for any other
  statement."
  [text tokens]
  (when-let [[value index] (parse-value text tokens (declaration-start tokens))]
    (when (and (= :call (:type value))
               (= index (count tokens)))
      value)))

(defn- parse-call
  "Finds the first call named `call-name` in `text`. `text` is a call or
  CSS. The function skips the declarations that are not such a call,
  such as the fallback color and the vendor-prefixed lines that CSS
  tools write. Gives nil when there is no such call."
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
  "The CSS color functions that the bundled schemas take. Each function
  has a name for three channels, and a name for three channels and an
  alpha."
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
  "Gives the angle in degrees, as a number, when the first argument of a
  gradient is its direction, such as `to right` or `0.25turn`. Gives nil
  when the argument is a color stop."
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
  "Writes a color in the form that tokenscript takes. A call of a CSS
  color function gets commas and loses the slash before its alpha. With
  an alpha, the call becomes the `rgba` or `hsla` of CSS. Other calls
  and references stay as written. Gives nil for anything that is not a
  color."
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
  "Gives the percentage of a position. `0` means `0%`."
  [{:keys [type value unit]}]
  (when (= :number type)
    (cond
      (= "%" unit)                   value
      (and (nil? unit) (zero? value)) 0)))

(defn- position-of
  "Reads a stop position: a percentage, or a reference to a percentage."
  [value]
  (or (position-percent value)
      (when (= :reference (:type value))
        (:text value))))

(defn- arg->stops
  "Gives the color stops of an argument. A color with one position or
  with none gives one stop. A color with two positions gives two stops.
  A color hint, a lone percentage, gives no stop. Gives nil for any
  other argument. A stop without a position has a nil position."
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
  "Gives a position to each stop that has none, as CSS does:

  - The first stop gets 0%.
  - The last stop gets 100%.
  - The other stops get positions spread evenly between the nearest
    stops that have a position."
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
  "Moves a position outside the gradient to the nearest end of the
  gradient."
  [position]
  (if (number? position)
    (str (format-number (max 0 (min 100 position))) "%")
    position))

(defn linear-gradient
  "Reads a CSS or tokenscript `linear-gradient()` as the value of a
  `gradient` token. The text can also be a `background` declaration, or
  several declarations as CSS tools copy them. The value is a map with
  its `\"kind\"`, its `\"angle\"` in degrees and its `\"stops\"`. Each
  stop has a `\"color\"` and a `\"position\"` in percent. The namespace
  docstring lists the rewrites. Gives nil for any other text, such as a
  radial gradient, a repeating gradient or a stop with a position in
  pixels."
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
  "Writes a CSS `linear-gradient()` as a tokenscript `linear-gradient()`.
  Gives nil when `text` is not a linear gradient."
  [text]
  (some-> (linear-gradient text)
          (gradient->tokenscript)))
