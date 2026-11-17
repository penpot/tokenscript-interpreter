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
   [tokenscript.util :as u]))

;; Tokenizer ------------------------------------------------------------------

(defn- code-at
  [text index]
  #?(:clj  (int (.charAt ^String text (int index)))
     :cljs (.charCodeAt ^string text index)))

(defn- digit?
  [code]
  (<= 48 code 57))

(defn- letter?
  [code]
  (or (<= 65 code 90) (<= 97 code 122)))

(defn- hex-digit?
  [code]
  (or (digit? code) (<= 65 code 70) (<= 97 code 102)))

(defn- space?
  [code]
  (or (= 32 code) (<= 9 code 13)))

(defn- name-char?
  [code]
  (or (letter? code) (digit? code) (= 45 code) (= 95 code)))

(defn- skip-while
  "The first index from `start` whose character `pred?` doesn't hold for."
  [text start pred?]
  (loop [index start]
    (if (and (< index (count text)) (pred? (code-at text index)))
      (recur (inc index))
      index)))

(defn- number-start?
  "Whether a number starts at `index`: digits, or a dot or a sign
  followed by them."
  [text index]
  (let [code   (code-at text index)
        second (when (< (inc index) (count text)) (code-at text (inc index)))
        third  (when (< (+ index 2) (count text)) (code-at text (+ index 2)))]
    (or (digit? code)
        (and (= 46 code) (some? second) (digit? second))
        (and (or (= 43 code) (= 45 code))
             (some? second)
             (or (digit? second)
                 (and (= 46 second) (some? third) (digit? third)))))))

(defn- read-number
  "The number at `index` with its unit, if it has one (`50%`, `90deg`)."
  [text index]
  (let [sign-end   (if (or (= 43 (code-at text index)) (= 45 (code-at text index)))
                     (inc index)
                     index)
        int-end    (skip-while text sign-end digit?)
        has-dot?   (and (< (inc int-end) (count text))
                        (= 46 (code-at text int-end))
                        (digit? (code-at text (inc int-end))))
        number-end (if has-dot?
                     (skip-while text (inc int-end) digit?)
                     int-end)
        unit-end   (if (and (< number-end (count text)) (= 37 (code-at text number-end)))
                     (inc number-end)
                     (skip-while text number-end letter?))
        unit       (subs text number-end unit-end)]
    {:type  :number
     :value (parse-double (subs text index number-end))
     :unit  (when-not (str/blank? unit) (str/lower-case unit))
     :start index
     :end   unit-end}))

(defn- read-reference
  "The token reference at `index`, up to its closing brace. Nil when it
  is never closed."
  [text index]
  (loop [position (inc index)
         depth    1]
    (when (< position (count text))
      (let [code (code-at text position)]
        (cond
          (= 123 code) (recur (inc position) (inc depth))
          (= 125 code) (if (= 1 depth)
                         {:type :reference :start index :end (inc position)}
                         (recur (inc position) (dec depth)))
          :else        (recur (inc position) depth))))))

(defn- read-token
  "The token at `index`, nil when the text can't be one."
  [text index]
  (let [code (code-at text index)]
    (cond
      (number-start? text index)
      (read-number text index)

      (or (letter? code) (= 95 code) (= 45 code))
      {:type :name :start index :end (skip-while text index name-char?)}

      (= 35 code)
      {:type :hash :start index :end (skip-while text (inc index) hex-digit?)}

      (= 123 code)
      (read-reference text index)

      ;; Strings and escapes are not part of a gradient
      (or (= 34 code) (= 39 code) (= 92 code))
      nil

      :else
      {:type :punctuation :start index :end (inc index)})))

(defn- tokenize
  "Cuts `text` into tokens, each with the range of the text it is. Nil
  when the text holds something a gradient doesn't."
  [text]
  (loop [index  0
         tokens []]
    (let [index (skip-while text index space?)]
      (if (>= index (count text))
        tokens
        (when-let [token (read-token text index)]
          (recur (:end token)
                 (conj tokens (assoc token :text (subs text (:start token) (:end token))))))))))

;; Parser ---------------------------------------------------------------------

(defn- punctuation?
  [token text]
  (and (= :punctuation (:type token))
       (= text (:text token))))

(defn- parse-value
  "The value at `index`: a call with its arguments, or a single token.
  Returns it with the index after it, nil when the tokens are no value."
  [text tokens index]
  (let [token (get tokens index)
        open  (get tokens (inc index))]
    (cond
      (nil? token)
      nil

      ;; A call, with the values of each of its arguments
      (and (= :name (:type token)) (some? open) (punctuation? open "("))
      (loop [position (+ index 2)
             current  []
             args     []]
        (when-let [argument (get tokens position)]
          (cond
            (punctuation? argument ")")
            [{:type :call
              :name (str/lower-case (:text token))
              :args (conj args current)
              :text (subs text (:start token) (:end argument))}
             (inc position)]

            (punctuation? argument ",")
            (recur (inc position) [] (conj args current))

            :else
            (when-let [[value next-position] (parse-value text tokens position)]
              (recur next-position (conj current value) args)))))

      (or (punctuation? token "(") (punctuation? token ")") (punctuation? token ","))
      nil

      :else
      [token (inc index)])))

(defn- declaration-start
  "The index the value starts at: after `background:` or
  `background-image:` when it is pasted as a CSS declaration."
  [tokens]
  (let [[property colon] tokens]
    (if (and (= :name (:type property))
             (contains? #{"background" "background-image"} (str/lower-case (:text property)))
             (some? colon)
             (punctuation? colon ":"))
      2
      0)))

(defn- parse-call
  "The call `text` is, also as a CSS declaration (`background: ...;`).
  Nil when it is anything else."
  [text]
  (when-let [tokens (tokenize text)]
    (when-let [[value index] (parse-value text tokens (declaration-start tokens))]
      (let [rest-tokens (subvec tokens index)]
        (when (and (= :call (:type value))
                   (or (empty? rest-tokens)
                       (and (= 1 (count rest-tokens))
                            (punctuation? (first rest-tokens) ";"))))
          value)))))

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
  `background` declaration, as the value of a `gradient` token: a map
  with its `\"kind\"`, its `\"angle\"` in degrees and its `\"stops\"`, each
  with a `\"color\"` and a `\"position\"` in percent. See the namespace
  for what is rewritten. Returns nil for anything else, like a radial or
  a repeating gradient or a stop positioned in pixels."
  [text]
  (when (string? text)
    (when-let [{:keys [name args]} (parse-call text)]
      (when (= "linear-gradient" name)
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
                               (spread-positions stops))}))))))))

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
