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

(t/deftest rotation-test
  (t/is (= {"plain" [] "deg" [] "turn" [] "rad" [] "grad" [] "word" ["INVALID_ROTATION_TYPE"]}
           (codes {"plain" ["90" "rotation"] "deg" ["90deg" "rotation"]
                   "turn" ["0.25turn" "rotation"] "rad" ["1rad" "rotation"]
                   "grad" ["100grad" "rotation"] "word" ["quarter" "rotation"]}))))

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

(t/deftest gradient-test
  (let [stops [{"color" "#f00" "position" "0"} {"color" "{blue}" "position" "100%"}]
        gradient (fn [fields] [(merge {"kind" "linear" "stops" stops} fields) "gradient"])
        with-stop (fn [stop] (gradient {"stops" (conj stops stop)}))
        issues (fn [tokens]
                 (-> (issues (assoc tokens "blue" ["#00f" "color"]))
                     (dissoc "blue")
                     (update-vals (fn [is] (mapv (fn [[code path]] [code (vec path)]) is)))))]
    (t/is (= {"plain" [] "angled" [] "round" [] "alias" [] "function" []}
             (issues {"plain" (gradient {})
                      "angled" (gradient {"angle" "45 * 2"})
                      "round" (gradient {"kind" "radial"})
                      "alias" ["{plain}" "gradient"]
                      "function" ["linear-gradient(90deg, {blue} 0%, #f00 100%)" "gradient"]})))
    (t/testing "the value is a dictionary with a kind"
      (t/is (= {"number" [["INVALID_GRADIENT_TYPE" []]]
                "word" [["INVALID_GRADIENT_TYPE" []]]
                "list" [["INVALID_GRADIENT_TYPE" []]]
                "cone" [["INVALID_GRADIENT_KIND" []]]
                "no-kind" [["INVALID_GRADIENT_KIND" []]]}
               (issues {"number" ["8" "gradient"]
                        "word" ["linear" "gradient"]
                        "list" [stops "gradient"]
                        "cone" (gradient {"kind" "conic"})
                        "no-kind" [{"stops" stops} "gradient"]}))))
    (t/testing "at least two stops, each with a color and a position"
      (t/is (= {"one" [["INVALID_GRADIENT_STOPS" []]]
                "no-stops" [["INVALID_GRADIENT_STOPS" []]]
                "no-color" [["INVALID_GRADIENT_STOPS" []]]
                "no-position" [["INVALID_GRADIENT_STOPS" []]]}
               (issues {"one" (gradient {"stops" (subvec stops 0 1)})
                        "no-stops" [{"kind" "linear"} "gradient"]
                        "no-color" (with-stop {"position" "1"})
                        "no-position" (with-stop {"color" "#fff"})}))))
    (t/testing "positions go from 0 to 1, or from 0% to 100%"
      (t/is (= {"half" [] "percent" []
                "big" [["INVALID_GRADIENT_STOP_POSITION" []]]
                "big-percent" [["INVALID_GRADIENT_STOP_POSITION" []]]
                "px" [["INVALID_GRADIENT_STOP_POSITION" []]]
                "not-a-number" [["INVALID_GRADIENT_STOP_POSITION" []]]}
               (issues {"half" (with-stop {"color" "#fff" "position" "0.5"})
                        "percent" (with-stop {"color" "#fff" "position" "50%"})
                        "big" (with-stop {"color" "#fff" "position" "1.5"})
                        "big-percent" (with-stop {"color" "#fff" "position" "150%"})
                        "px" (with-stop {"color" "#fff" "position" "8px"})
                        "not-a-number" (with-stop {"color" "#fff" "position" "#fff"})}))))
    (t/testing "the angle is a rotation in any angle unit"
      (t/is (= {"deg" [] "turn" [] "rad" [] "grad" []}
               (issues {"deg" (gradient {"angle" "45deg"})
                        "turn" (gradient {"angle" "0.25turn"})
                        "rad" (gradient {"angle" "1rad"})
                        "grad" (gradient {"angle" "100grad"})}))))
    (t/testing "the angle and the stop colors fail with the codes of their types"
      (t/is (= {"bad-angle" [["INVALID_ROTATION_TYPE" ["angle"]]]
                "bad-color" [["INVALID_COLOR_TYPE" ["stops" 2 "color"]]]}
               (issues {"bad-angle" (gradient {"angle" "#fff"})
                        "bad-color" (with-stop {"color" "8" "position" "1"})}))))))

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

(t/deftest script-failures-test
  (let [with-script (fn [script] (update (tsc/default-config) :tokens token-types/register-validation "opacity" script))
        result (fn [cfg] (-> (validation/validate cfg "opacity" (sym/number 2)) first (select-keys [:error :data])))
        unparsable (with-script "return (;")
        raising (with-script "return {input} / 0;")
        not-a-code (with-script "return 5;")]
    (t/is (= {:error "VALIDATION_INVALID_SCRIPT" :data {:script "return (;"}} (result unparsable)))
    (t/is (= {:error "VALIDATION_INVALID_RESULT"} (result not-a-code)))
    (t/testing "a raising script keeps its error as the cause"
      (t/is (= "VALIDATION_ERROR" (:error (result raising))))
      (t/is (= :operations/division-by-zero (get-in (result raising) [:data :cause :code])))
      (t/is (= (result raising) (result raising)) "the cached result keeps the data"))
    (t/testing "issues carry the data and no message"
      (let [issue (get-in (tsc/build {"x" {:value "2" :type "opacity"}} {:config unparsable}) [:issues "x" 0])]
        (t/is (= {:type :tokenscript/issue :code "VALIDATION_INVALID_SCRIPT" :severity :warning
                  :token-name "x" :path nil :data {:script "return (;" :token-type "opacity"}}
                 issue))))))

(t/deftest error-types-test
  (t/is (= ["INVALID_NUMBER_UNIT" "INVALID_NUMBER_TYPE"]
           (token-types/validation-error-types (:tokens (tsc/default-config)) "number"))))
