;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns tokenscript.document.dtcg
  "What a DTCG token file means: which nodes are tokens, its type names,
  JSON pointers, and the object values of the Format Module 2025.10, read
  as tokenscript source and written back.

  Read, the object values become strings, so that tokenscript resolves
  them and reports their errors (a missing token, a wrong type) as it
  does for any token: `{value: 16, unit: \"px\"}` is `\"16px\"`, a color
  object is the color in its color space, `oklch(0.628, 0.2577, 29.23)`,
  and a JSON pointer is a reference. Strings, numbers and booleans are
  kept. Composite values (typography, shadow) keep their structure;
  their fields are read the same way. Shadows are normalized to a vector
  of objects with the DTCG keys. Written, literal dimensions, colors and
  numbers become DTCG objects again; expressions and references stay
  strings.

  See docs/dtcg.md."
  (:require
   [clojure.string :as str]
   [tokenscript.colors :as colors]
   [tokenscript.core :as tsc]
   [tokenscript.errors :as err]
   [tokenscript.interpreter :as interp]
   [tokenscript.parser :as parser]
   [tokenscript.registry :as reg]
   [tokenscript.symbols :as sym]
   [tokenscript.util :as u]))

(defn- field
  "Value of the string key `k` in `m`, also when the keys are keywords."
  [m k]
  (if (contains? m k) (get m k) (get m (keyword k))))

;; Types ----------------------------------------------------------------------

(def ^:private spec-type->type
  "DTCG 2025.10 type names that differ from the names the bundled token
  types validate."
  {"fontFamily" "fontFamilies"
   "fontWeight" "fontWeights"
   "boxShadow" "shadow"})

(defn read-type
  "The token type of a file, by the names of the bundled token types
  where the DTCG name differs."
  [type]
  (get spec-type->type type type))

(def ^:private type->spec-type
  "The type names of the bundled token types to DTCG 2025.10 type names,
  for writing."
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

;; Tokens ---------------------------------------------------------------------

(defn property?
  "Keys starting with `$` are properties of their token or group, never
  the names of tokens or groups."
  [k]
  (and (string? k) (str/starts-with? k "$")))

(defn token?
  "A DTCG token has a `$value`, or a `$ref` and no tokens or groups of
  its own (a JSON pointer alias)."
  [node]
  (and (map? node)
       (or (contains? node "$value")
           (and (contains? node "$ref") (every? property? (keys node))))))

(defn legacy-token?
  "Tokens from before DTCG: `value` and a string `type`, without `$`. A
  group with tokens named `value` and `type` is not one."
  [node]
  (let [value (get node "value")]
    (and (map? node)
         (string? (get node "type"))
         (contains? node "value")
         (not (and (map? value) (contains? value "$value")))
         (not (contains? node "$value")))))

;; References -----------------------------------------------------------------

(defn- invalid-name?
  "Token and group names cannot contain `.`, `{` or `}` (DTCG 2025.10,
  5.1.1): references would read them as a path or a reference."
  [name]
  (and (string? name) (some? (re-find #"[.{}]" name))))

(defn pointer->path
  "The token or group name a JSON pointer names: `#/colors/blue` and
  `#/colors/blue/$value` are `colors.blue`. Nil for a pointer into a
  value (`#/colors/blue/$value/components/0`), which tokenscript has no
  reference for, and for one through an invalid name (`#/a.b`)."
  [pointer]
  (when-let [[_ path] (and (string? pointer) (re-matches #"^#/(.+?)(?:/\$value)?$" pointer))]
    (let [segments (map #(-> % (str/replace "~1" "/") (str/replace "~0" "~"))
                        (str/split path #"/"))]
      (when-not (some #(or (str/starts-with? % "$") (invalid-name? %)) segments)
        (str/join "." segments)))))

(defn pointer->reference
  "The tokenscript reference of a JSON pointer alias: `#/colors/blue` is
  `{colors.blue}`."
  [pointer]
  (when-let [path (pointer->path pointer)]
    (str "{" path "}")))

(defn- read-pointer
  [pointer path]
  (if-let [reference (pointer->reference pointer)]
    {:value reference}
    {:error {:reason :unsupported-pointer :pointer pointer :path path}}))

;; Reading values -------------------------------------------------------------

(defn- pointer-object?
  "A JSON pointer alias in place of a value or one of its fields:
  `{\"$ref\": \"#/colors/blue/$value\"}`.
  https://www.designtokens.org/tr/2025.10/format/#x7-1-2-json-pointer-syntax-required-support"
  [v]
  (and (map? v) (contains? v "$ref")))

(defn- unit-object?
  "A dimension or a duration, which share one shape: `{value: 16, unit:
  \"px\"}`, `{value: 200, unit: \"ms\"}`.
  https://www.designtokens.org/tr/2025.10/format/#x8-2-dimension
  https://www.designtokens.org/tr/2025.10/format/#x8-5-duration"
  [v]
  (and (map? v)
       (= 2 (count v))
       (number? (field v "value"))
       (string? (field v "unit"))))

(defn- color-object?
  "A color as a color space and its components, with an optional
  alpha and hex fallback.
  https://www.designtokens.org/tr/2025.10/color/#x4-1-format"
  [v]
  (and (map? v)
       (string? (field v "colorSpace"))
       (sequential? (field v "components"))))

(def ^:private color-spaces
  "DTCG color spaces by the tokenscript color space with the same
  channels in the same order. HSL saturation and lightness are
  percentages in DTCG and fractions in tokenscript; they are passed as
  `%` to the CSS `hsl()` function of the schemas, which reads them. HWB
  has the same difference and no such function; A98 RGB, ProPhoto RGB
  and Rec. 2020 have no color space in the schemas.
  https://www.designtokens.org/tr/2025.10/color/#supported-color-spaces"
  {"srgb" {:space "SRGB"}
   "srgb-linear" {:space "LinearSRGB"}
   "hsl" {:space "HSL" :percent #{1 2}}
   "lab" {:space "Lab"}
   "lch" {:space "LCH"}
   "oklab" {:space "OKLab"}
   "oklch" {:space "OKLCH"}
   "display-p3" {:space "P3"}
   "xyz-d65" {:space "XYZD65"}
   "xyz-d50" {:space "XYZD50"}})

(defn- color-space
  "The color space of the schemas a DTCG color space is read into: the
  keyword that creates its colors and its channels. Nil when the
  schemas don't define it."
  [dtcg-space]
  (when-let [{:keys [space percent]} (get color-spaces dtcg-space)]
    (let [spec (reg/spec-by-name (:colors (tsc/default-config)) space)]
      (when-let [keyword (-> spec :initializers first :keyword)]
        {:dtcg-space dtcg-space
         :keyword keyword
         :channels (get-in spec [:schema :order])
         :percent percent}))))

(defn- color-hex
  "Hex of a color by the conversions of the schemas, nil when they have
  none to hex."
  [color]
  (err/language-result #(sym/to-string (tsc/color->hex color))))

(defn- with-alpha
  [hex alpha]
  (if (sym/transparent? alpha) (sym/format-hex8 hex alpha) hex))

(defn- color-expression
  "The token value of a color in a color space of the schemas:
  `oklch(0.628, 0.2577, 29.23, 0.5)`."
  [{:keys [keyword percent]} components alpha]
  (let [channels (map-indexed (fn [i c]
                                (str (u/num->str (if (number? c) c 0))
                                     (when (contains? percent i) "%")))
                              components)]
    (str keyword "("
         (str/join ", " (cond-> (vec channels) (sym/transparent? alpha) (conj (u/num->str alpha))))
         ")")))

(defn- hex-of-components?
  "Whether `hex` is exactly the sRGB color of `components`, as `write`
  gives both for a hex color."
  [{:keys [dtcg-space channels]} hex components]
  (and (= "srgb" dtcg-space)
       (let [hex (:color (sym/parse-hex-with-alpha hex))
             srgb (sym/value (interp/color->type (tsc/default-config) (sym/color hex) "SRGB"))]
         (every? true? (map (fn [channel c]
                              (and (number? c) (== c (sym/value (get srgb channel)))))
                            channels components)))))

(defn- valid-components?
  [{:keys [channels]} components]
  (and (sequential? components)
       (= (count channels) (count components))
       (every? #(or (number? %) (= "none" %)) components)))

(defn- read-color
  "A color of a file in its color space, `oklch(0.628, 0.2577, 29.23)`.
  The file's `hex` stands in for a color space the schemas don't have,
  with a warning, and is kept when it is exactly the color."
  [v]
  (let [space-name (field v "colorSpace")
        components (field v "components")
        alpha (field v "alpha")
        hex (let [hex (field v "hex")] (when (and (string? hex) (sym/valid-hex? hex)) hex))
        space (color-space space-name)]
    (cond
      (and (some? alpha) (not (and (number? alpha) (<= 0 alpha 1))))
      {:error {:reason :invalid-alpha :alpha alpha}}

      (and (nil? space) hex)
      {:value (with-alpha hex alpha)
       :warnings [{:code :dtcg/unsupported-color-space :color-space space-name}]}

      (nil? space)
      {:error {:reason :unsupported-color-space :color-space space-name}}

      (not (valid-components? space components))
      {:error {:reason :invalid-components :color-space space-name}}

      (and hex (hex-of-components? space hex components))
      {:value (with-alpha hex alpha)}

      :else
      {:value (color-expression space components alpha)})))

(defn- read-leaf
  "A value read as tokenscript reads it, or `{:error reason}`."
  [v]
  (cond
    (unit-object? v) {:value (str (u/num->str (field v "value")) (field v "unit"))}
    (color-object? v) (read-color v)
    :else {:value v}))

(defn- add-warnings
  [acc {:keys [warnings]}]
  (cond-> acc (seq warnings) (update :warnings (fnil into []) warnings)))

(defn- read-tree
  "Reads the leaves of a composite value. Returns `{:value v}` with the
  `:warnings` of its leaves, or the first `{:error reason}`; both have
  the field path."
  [v path]
  (cond
    (pointer-object? v)
    (read-pointer (get v "$ref") path)

    (or (unit-object? v) (color-object? v))
    (let [r (read-leaf v)]
      (cond-> r
        (:error r) (update :error assoc :path path)
        (:warnings r) (update :warnings (partial mapv #(assoc % :path path)))))

    (map? v)
    (reduce (fn [acc [k child]]
              (let [r (read-tree child (conj path k))]
                (if (:error r)
                  (reduced r)
                  (-> acc (assoc-in [:value k] (:value r)) (add-warnings r)))))
            {:value (empty v)}
            v)

    (sequential? v)
    (reduce (fn [acc [i child]]
              (let [r (read-tree child (conj path i))]
                (if (:error r)
                  (reduced r)
                  (-> acc (update :value conj (:value r)) (add-warnings r)))))
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
  "A shadow object with the DTCG keys only. The legacy `x`, `y` and
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
  "Reads the value `v` of a token of `type` (a type name of the bundled
  token types, see `read-type`). Returns `{:value v}`, with `:warnings`
  for colors read as their hex, or `{:error reason}`."
  [type v]
  (read-tree (if (= "shadow" type) (read-shadow v) v) []))

;; Writing values -------------------------------------------------------------

(defn- number-node?
  [{:keys [node] :as ast}]
  (case node
    :num true
    :element-with-unit (= :num (:node (:ast-node ast)))
    :unary-op (and (= "-" (:value (:op-token ast))) (number-node? (:expr ast)))
    false))

(defn- literal-number
  "The number of a literal number or dimension (`1.5`, `16px`, `-4px`).
  Nil for references and other expressions, which stay strings."
  [v]
  (when-let [ast (and (string? v) (err/language-result #(:ast (parser/parse v {:mode :inline}))))]
    (when (number-node? ast)
      (err/language-result #(interp/interpret ast)))))

(defn- json-number
  "Whole numbers as integers, so the file says `16`, not `16.0`."
  [n]
  #?(:clj (if (and (u/integer-value? n) (< (Math/abs (double n)) 1e15)) (long n) n)
     :cljs n))

(def ^:private dimension-units
  "The units of a DTCG 2025.10 dimension."
  #{"px" "rem"})

(defn- write-dimension
  [v]
  (let [value (literal-number v)]
    (cond
      (number? v) {"value" v "unit" "px"}
      (sym/number? value) {"value" (json-number (:value value)) "unit" "px"}
      (and (sym/number-with-unit? value) (contains? dimension-units (:unit value)))
      {"value" (json-number (:value value)) "unit" (:unit value)}
      :else v)))

(defn- write-number
  [v]
  (let [value (literal-number v)]
    (if (sym/number? value) (json-number (:value value)) v)))

(def ^:private written-color-spaces
  "DTCG color spaces by the tokenscript color space a color is written
  in as it is: those of `color-spaces` with the same channel ranges."
  (into {}
        (keep (fn [[dtcg-space {:keys [space percent]}]]
                (when-not percent [(u/name-key space) dtcg-space])))
        color-spaces))

(defn- color-call?
  "A call of a color space keyword with numbers only, as `read` writes
  colors: `oklch(0.628, 0.2577, 29.23)`."
  [{:keys [node name args]}]
  (and (= :function-call node)
       (some? (colors/initializer (:colors (tsc/default-config)) name))
       (every? number-node? args)))

(defn- literal-color
  "The color of a hex color or a color call. Nil for references and
  other expressions, which stay strings."
  [v]
  (when-let [ast (and (string? v) (err/language-result #(:ast (parser/parse v {:mode :inline}))))]
    (when (or (= :hex-color (:node ast)) (color-call? ast))
      (err/language-result #(interp/interpret ast {:config (tsc/default-config)})))))

(defn- color-object
  "A color in its own color space when DTCG has it, else in sRGB, with
  its hex as the fallback."
  [color]
  (let [own (get written-color-spaces (u/name-key (:sub-type color)))
        color (if own color (interp/color->type (tsc/default-config) color "SRGB"))
        {:keys [dtcg-space channels]} (color-space (or own "srgb"))
        hex (color-hex (assoc color :alpha nil))]
    (cond-> {"colorSpace" dtcg-space
             "components" (mapv #(json-number (sym/value (get (sym/value color) %))) channels)}
      hex (assoc "hex" hex)
      (sym/transparent? (:alpha color)) (assoc "alpha" (:alpha color)))))

(defn- write-color
  [v]
  (or (when-let [color (literal-color v)]
        (err/language-result #(color-object color)))
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
  "Writes the value `v` of a token of `type` (a type name of the bundled
  token types) as DTCG 2025.10: literal dimensions, colors and numbers
  become their DTCG values; references and expressions stay strings."
  [type v]
  (case (write-type type)
    "dimension" (write-dimension v)
    "color" (write-color v)
    ("number" "fontWeight" "opacity") (write-number v)
    "typography" (write-typography v)
    "shadow" (write-shadow v)
    v))
