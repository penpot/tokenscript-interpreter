;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns tokenscript.token-types-test
  "The bundled token type schemas, validated through `tokenscript.core/build`."
  (:require
   [clojure.test :as t]
   [tokenscript.core :as tsc]
   [tokenscript.symbols :as sym]
   [tokenscript.token-types :as token-types]
   [tokenscript.validation :as validation]))

(defn- issues
  "`{name [[code path] ..]}` of the validation issues of `tokens`
  (`{name [value type]}`), built with the bundled schemas."
  [tokens]
  (let [res (tsc/build (into {} (map (fn [[k [v type]]] [k (cond-> {:value v} type (assoc :type type))])) tokens))]
    (into {} (map (fn [k] [k (mapv (juxt :code :path) (get-in res [:issues k]))])) (keys tokens))))

(defn- codes
  [tokens]
  (update-vals (issues tokens) #(mapv first %)))

(t/deftest numeric-types-test
  (t/is (= {"ok" [] "plain" [] "rem" [] "glued" ["INVALID_DIMENSION_TYPE"]
            "list" ["INVALID_DIMENSION_TYPE"] "color" ["INVALID_DIMENSION_TYPE"]}
           (codes {"ok" ["8px" "dimension"]
                   "plain" ["8" "dimension"]
                   "rem" ["{plain} * 1rem" "dimension"]
                   "glued" ["1foo" "dimension"]
                   "list" ["1 2" "dimension"]
                   "color" ["#fff" "dimension"]})))
  (t/testing "every numeric type has its own code"
    (t/is (= {"a" ["INVALID_SIZING_TYPE"] "b" ["INVALID_SPACING_TYPE"]
              "c" ["INVALID_BORDER_RADIUS_TYPE"] "d" ["INVALID_FONT_SIZE_TYPE"]
              "e" ["INVALID_LETTER_SPACING_TYPE"] "f" ["INVALID_LINE_HEIGHT_TYPE"]
              "g" ["INVALID_ROTATION_TYPE"]}
             (codes {"a" ["x" "sizing"] "b" ["x" "spacing"] "c" ["x" "borderRadius"]
                     "d" ["x" "fontSizes"] "e" ["x" "letterSpacing"] "f" ["x" "lineHeights"]
                     "g" ["x" "rotation"]}))))
  (t/testing "type names are case insensitive"
    (t/is (= {"x" ["INVALID_DIMENSION_TYPE"]} (codes {"x" ["1foo" "Dimension"]})))))

(t/deftest number-test
  (t/is (= {"ok" [] "unit" ["INVALID_NUMBER_UNIT"] "string" ["INVALID_NUMBER_TYPE"]}
           (codes {"ok" ["1.5" "number"] "unit" ["10px" "number"] "string" ["abc" "number"]}))))

(t/deftest border-width-test
  (t/is (= {"ok" [] "negative" ["INVALID_BORDER_WIDTH_NEGATIVE"] "string" ["INVALID_BORDER_WIDTH_TYPE"]}
           (codes {"ok" ["2px" "borderWidth"] "negative" ["-1" "borderWidth"] "string" ["thin" "borderWidth"]}))))

(t/deftest opacity-test
  (t/is (= {"ok" [] "percent" [] "big" ["INVALID_OPACITY_RANGE"] "big-percent" ["INVALID_OPACITY_RANGE"]
            "negative" ["INVALID_OPACITY_RANGE"] "px" ["INVALID_OPACITY_TYPE"]}
           (codes {"ok" ["0.5" "opacity"] "percent" ["50%" "opacity"] "big" ["2" "opacity"]
                   "big-percent" ["150%" "opacity"] "negative" ["-0.5" "opacity"] "px" ["1px" "opacity"]}))))

(t/deftest color-test
  (t/is (= {"hex" [] "function" [] "number" ["INVALID_COLOR_TYPE"]}
           (codes {"hex" ["#fff" "color"] "function" ["lighten(#ff0000, 0.1)" "color"] "number" ["8" "color"]}))))

(t/deftest text-test
  (t/is (= {"case" [] "case-upper" [] "case-bad" ["INVALID_TEXT_CASE_VALUE"]
            "decoration" [] "decoration-number" ["INVALID_TEXT_DECORATION_TYPE"]}
           (codes {"case" ["uppercase" "textCase"] "case-upper" ["Capitalize" "textCase"]
                   "case-bad" ["wat" "textCase"]
                   "decoration" ["underline" "textDecoration"] "decoration-number" ["5" "textDecoration"]}))))

(t/deftest fonts-test
  (t/is (= {"name" [] "number" [] "words" [] "big" ["INVALID_FONT_WEIGHT_RANGE"]
            "family" [] "families" [] "family-number" ["INVALID_FONT_FAMILY_TYPE"]}
           (codes {"name" ["bold" "fontWeights"] "number" ["700" "fontWeights"]
                   "words" ["700 italic" "fontWeights"] "big" ["1200" "fontWeights"]
                   "family" ["Inter" "fontFamilies"] "families" ["Inter, sans-serif" "fontFamilies"]
                   "family-number" ["8" "fontFamilies"]}))))

(t/deftest typography-test
  (t/is (= {"ok" [] "bad-field" [["INVALID_FONT_SIZE_TYPE" ["fontSize"]]]
            "number" [["INVALID_TYPOGRAPHY_TYPE" []]]}
           (update-vals (issues {"ok" [{"fontSize" "16" "lineHeight" "1.5" "fontWeight" "bold"} "typography"]
                                 "bad-field" [{"fontSize" "1foo" "lineHeight" "1.5"} "typography"]
                                 "number" ["8" "typography"]})
                        (fn [is] (mapv (fn [[code path]] [code (vec path)]) is)))))
  (t/testing "field names match in kebab case too"
    (t/is (= {"x" [["INVALID_LINE_HEIGHT_TYPE" ["lineHeight"]]]}
             (issues {"x" [{"font-size" "16" "line-height" "#fff"} "typography"]})))))

(t/deftest shadow-test
  (let [shadow (fn [blur] [{"offsetX" "0" "offsetY" "4" "blur" blur "spread" "0" "color" "#000"}])]
    (t/is (= {"ok" [] "bad-field" [["INVALID_DIMENSION_TYPE" [0 "blur"]]] "number" [["INVALID_SHADOW_TYPE" []]]}
             (update-vals (issues {"ok" [(shadow "8") "shadow"]
                                   "bad-field" [(shadow "#000") "shadow"]
                                   "number" ["8" "shadow"]})
                          (fn [is] (mapv (fn [[code path]] [code (vec path)]) is)))))))

(t/deftest untyped-test
  (t/is (= {"untyped" [] "unknown" []}
           (codes {"untyped" ["1foo" nil] "unknown" ["1foo" "wat"]}))))

(t/deftest cached-results-test
  (let [cfg (tsc/default-config)
        lenient (update cfg :tokens token-types/register-validation "opacity" "return true;")
        errors (fn [cfg v] (mapv :error (validation/validate cfg "opacity" (sym/number v))))]
    (t/is (= ["INVALID_OPACITY_RANGE"] (errors cfg 2)))
    (t/is (= ["INVALID_OPACITY_RANGE"] (errors cfg 2)) "from the cache")
    (t/testing "a new validation script is not answered from the old cache"
      (t/is (= [nil] (errors lenient 2)))
      (t/is (= ["INVALID_OPACITY_RANGE"] (errors cfg 2))))))

(t/deftest error-types-test
  (t/is (= ["INVALID_NUMBER_UNIT" "INVALID_NUMBER_TYPE"]
           (token-types/validation-error-types (:tokens (tsc/default-config)) "number"))))
