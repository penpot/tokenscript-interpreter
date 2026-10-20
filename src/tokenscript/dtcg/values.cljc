;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns tokenscript.dtcg.values
  "Token values between DTCG files and tokenscript token data.

  Reading turns the object values of the DTCG Format Module 2025.10
  into the strings tokenscript reads: `{value: 16, unit: \"px\"}` is
  `\"16px\"`, a color object is its hex color. Strings, numbers and
  booleans are kept. Composite values (typography, shadow) keep their
  structure; their fields are read the same way. Shadows are
  normalized to a vector of objects with the DTCG keys.

  Writing (the `:dtcg` profile of `tokenscript.dtcg/write`) does the
  reverse for literal values; expressions and references stay strings.

  See docs/dtcg.md."
  (:require
   [clojure.string :as str]
   [tokenscript.core :as tsc]
   [tokenscript.symbols :as sym]
   [tokenscript.util :as u]))

(defn- field
  "Value of the string key `k` in `m`, also when the keys are keywords."
  [m k]
  (if (contains? m k) (get m k) (get m (keyword k))))

;; Types ----------------------------------------------------------------------

(def ^:private spec-type->type
  "DTCG 2025.10 type names that differ from the Tokens Studio names the
  bundled token types validate."
  {"fontFamily" "fontFamilies"
   "fontWeight" "fontWeights"
   "boxShadow" "shadow"})

(defn read-type
  "The token type of a file, by the Tokens Studio names where the DTCG
  name differs."
  [type]
  (get spec-type->type type type))

(def ^:private type->spec-type
  "Tokens Studio type names to DTCG 2025.10 type names, for writing."
  {"fontFamilies" "fontFamily"
   "fontWeights" "fontWeight"
   "boxShadow" "shadow"
   "sizing" "dimension"
   "spacing" "dimension"
   "borderRadius" "dimension"
   "borderWidth" "dimension"
   "fontSizes" "dimension"
   "letterSpacing" "dimension"
   "lineHeights" "number"})

(defn write-type
  "The DTCG 2025.10 type name of a token type."
  [type]
  (get type->spec-type type type))

;; Reading --------------------------------------------------------------------

(defn- dimension-object?
  [v]
  (and (map? v)
       (= 2 (count v))
       (number? (field v "value"))
       (string? (field v "unit"))))

(defn- color-object?
  [v]
  (and (map? v)
       (string? (field v "colorSpace"))
       (sequential? (field v "components"))))

(def ^:private color-spaces
  "DTCG color spaces the bundled schemas convert to hex: the color space
  name in tokenscript, its channels and the factor from the DTCG range
  to the tokenscript range. HWB, Lab, LCH and XYZ-D50 have no conversion
  to hex in the bundled schemas, Display P3 converts without gamut
  mapping; a file has to give `hex` for them."
  {"srgb" ["SRGB" ["r" "g" "b"] [1 1 1]]
   "srgb-linear" ["LinearSRGB" ["r" "g" "b"] [1 1 1]]
   "hsl" ["HSL" ["h" "s" "l"] [1 0.01 0.01]]
   "oklab" ["OKLab" ["l" "a" "b"] [1 1 1]]
   "oklch" ["OKLCH" ["l" "c" "h"] [1 1 1]]
   "xyz-d65" ["XYZD65" ["x" "y" "z"] [1 1 1]]})

(defn- with-alpha
  [hex alpha]
  (if (sym/transparent? alpha) (sym/format-hex8 hex alpha) hex))

(defn- read-color
  "Hex color of a DTCG color object, or `{:error reason}`."
  [v]
  (let [space (field v "colorSpace")
        components (field v "components")
        alpha (field v "alpha")
        hex (field v "hex")]
    (cond
      (and (some? alpha) (not (and (number? alpha) (<= 0 alpha 1))))
      {:error {:reason :invalid-alpha :alpha alpha}}

      (and (string? hex) (sym/valid-hex? hex))
      {:value (with-alpha hex alpha)}

      (not (contains? color-spaces space))
      {:error {:reason :unsupported-color-space :color-space space}}

      :else
      (let [[sub-type channels factors] (get color-spaces space)]
        (if-not (and (= (count channels) (count components))
                     (every? #(or (number? %) (= "none" %)) components))
          {:error {:reason :invalid-components :color-space space}}
          (let [channel-values (map (fn [c f] (sym/number (* f (if (number? c) c 0))))
                                    components factors)
                color (sym/color (zipmap channels channel-values) sub-type alpha)]
            {:value (sym/to-string (tsc/color->hex color))}))))))

(defn- read-leaf
  "A value read as tokenscript reads it, or `{:error reason}`."
  [v]
  (cond
    (dimension-object? v) {:value (str (u/num->str (field v "value")) (field v "unit"))}
    (color-object? v) (read-color v)
    :else {:value v}))

(defn- read-tree
  "Reads the leaves of a composite value. Returns `{:value v}` or the
  first `{:error reason}`, with the field path in the reason."
  [v path]
  (cond
    (or (dimension-object? v) (color-object? v))
    (let [r (read-leaf v)]
      (cond-> r (:error r) (update :error assoc :path path)))

    (map? v)
    (reduce (fn [acc [k child]]
              (let [r (read-tree child (conj path k))]
                (if (:error r)
                  (reduced r)
                  (assoc-in acc [:value k] (:value r)))))
            {:value (empty v)}
            v)

    (sequential? v)
    (reduce (fn [acc [i child]]
              (let [r (read-tree child (conj path i))]
                (if (:error r)
                  (reduced r)
                  (update acc :value conj (:value r)))))
            {:value []}
            (map-indexed vector v))

    :else
    {:value v}))

(def ^:private shadow-keys
  {"x" "offsetX" "offsetX" "offsetX"
   "y" "offsetY" "offsetY" "offsetY"
   "blur" "blur" "spread" "spread" "color" "color" "inset" "inset"})

(defn- read-inset
  [shadow]
  (let [inset (field shadow "inset")]
    (cond
      (boolean? inset) inset
      (= "true" inset) true
      (= "false" inset) false
      :else (= "innerShadow" (or (field shadow "type") (field shadow "$type"))))))

(defn- read-shadow-object
  "A shadow object with the DTCG keys only. Tokens Studio's `x`, `y` and
  `type: innerShadow` are read as `offsetX`, `offsetY` and `inset`."
  [shadow]
  (if-not (map? shadow)
    shadow
    (-> (reduce-kv (fn [acc k v]
                     (if-let [k' (get shadow-keys (if (keyword? k) (name k) k))]
                       (assoc acc k' v)
                       acc))
                   {}
                   shadow)
        (assoc "inset" (read-inset shadow)))))

(defn read-shadow
  "A shadow value as a vector of shadow objects; references are kept."
  [v]
  (cond
    (map? v) [(read-shadow-object v)]
    (sequential? v) (mapv read-shadow-object v)
    :else v))

(defn read-value
  "Reads the value `v` of a token of `type` (a Tokens Studio type name,
  see `read-type`). Returns `{:value v}` or `{:error reason}`."
  [type v]
  (read-tree (if (= "shadow" type) (read-shadow v) v) []))

;; Writing --------------------------------------------------------------------

(def ^:private dimension-re #"^(-?(?:\d+\.?\d*|\.\d+))(px|rem)?$")
(def ^:private number-re #"^-?(?:\d+\.?\d*|\.\d+)$")

(defn- parse-number
  [s]
  #?(:clj (let [d (Double/parseDouble s)]
            (if (and (== d (Math/floor d)) (< (Math/abs d) 1e15)) (long d) d))
     :cljs (js/parseFloat s)))

(defn- write-dimension
  [v]
  (if-let [[_ n unit] (and (string? v) (re-matches dimension-re (str/trim v)))]
    {"value" (parse-number n) "unit" (or unit "px")}
    (if (number? v) {"value" v "unit" "px"} v)))

(defn- write-number
  [v]
  (if (and (string? v) (re-matches number-re (str/trim v)))
    (parse-number (str/trim v))
    v))

(defn- channel
  [hex i]
  (/ (u/parse-int (subs hex (inc (* 2 i)) (+ 3 (* 2 i))) 16) 255.0))

(defn- round-to
  [n places]
  (let [f (Math/pow 10 places)]
    (/ (Math/round (* n f)) f)))

(defn- write-color
  [v]
  (if (and (string? v) (sym/valid-hex? v))
    (let [{:keys [color alpha]} (sym/parse-hex-with-alpha v)
          hex (str/lower-case (if (= 4 (count color))
                                (let [[_ r g b] (map str color)] (str "#" r r g g b b))
                                color))]
      (cond-> {"colorSpace" "srgb"
               "components" (mapv #(round-to (channel hex %) 4) (range 3))
               "hex" hex}
        (sym/transparent? alpha) (assoc "alpha" (round-to alpha 4))))
    v))

(def ^:private typography-fields
  "Typography field names to DTCG 2025.10, and how their values are
  written."
  {"fontFamily" ["fontFamily" identity]
   "fontFamilies" ["fontFamily" identity]
   "fontSize" ["fontSize" write-dimension]
   "fontSizes" ["fontSize" write-dimension]
   "fontWeight" ["fontWeight" write-number]
   "fontWeights" ["fontWeight" write-number]
   "letterSpacing" ["letterSpacing" write-dimension]
   "lineHeight" ["lineHeight" write-number]
   "lineHeights" ["lineHeight" write-number]})

(defn- write-typography
  [v]
  (if-not (map? v)
    v
    (reduce-kv (fn [acc k field-value]
                 (let [k (if (keyword? k) (name k) k)
                       [k' write] (get typography-fields k [k identity])]
                   (assoc acc k' (write field-value))))
               {}
               v)))

(defn- write-shadow
  [v]
  (if-not (sequential? v)
    v
    (mapv (fn [shadow]
            (if-not (map? shadow)
              shadow
              (reduce-kv (fn [acc k field-value]
                           (assoc acc k (case k
                                          ("offsetX" "offsetY" "blur" "spread") (write-dimension field-value)
                                          "color" (write-color field-value)
                                          field-value)))
                         {}
                         (read-shadow-object shadow))))
          v)))

(defn write-value
  "Writes the value `v` of a token of `type` (a Tokens Studio type name)
  as DTCG 2025.10: literal dimensions, colors and numbers become their
  DTCG values; references and expressions stay strings."
  [type v]
  (case (write-type type)
    "dimension" (write-dimension v)
    "color" (write-color v)
    ("number" "fontWeight" "opacity") (write-number v)
    "typography" (write-typography v)
    "shadow" (write-shadow v)
    v))
