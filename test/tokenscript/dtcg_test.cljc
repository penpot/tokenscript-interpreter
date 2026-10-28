;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns tokenscript.dtcg-test
  (:require
   [clojure.test :as t]
   [tokenscript.core :as tsc]
   [tokenscript.dtcg :as dtcg]
   [tokenscript.processor :as processor]
   [tokenscript.test-helpers :as th]))

(defn- tokens
  "The tokens of the set `set-name` of a read document."
  [result set-name]
  (->> (:sets result) (filter #(= set-name (:name %))) first :tokens))

(defn- codes
  [result]
  (frequencies (map :code (:issues result))))

;; Reading: formats -----------------------------------------------------------

(t/deftest read-legacy-tokens
  (let [result (dtcg/read {"color" {"red" {"value" "#ff0000" "type" "color"
                                           "description" "Red"}}
                           "space" {"sm" {"value" "4" "type" "spacing"}}}
                          {:set-name "core"})]
    (t/is (= :single-set (:document result)))
    (t/is (= {"color.red" {:value "#ff0000" :type "color" :description "Red"}
              "space.sm" {:value "4" :type "spacing"}}
             (tokens result "core")))
    (t/is (empty? (:issues result)))))

(t/deftest read-draft-tokens-with-group-types
  (let [result (dtcg/read {"colors" {"$type" "color"
                                     "red" {"$value" "#ff0000"}
                                     "brand" {"$value" "#0000ff" "$description" "Brand"}
                                     "label" {"$value" "Red" "$type" "string"}}}
                          {:set-name "core"})]
    (t/is (= {"colors.red" {:value "#ff0000" :type "color"}
              "colors.brand" {:value "#0000ff" :type "color" :description "Brand"}
              "colors.label" {:value "Red" :type "string"}}
             (tokens result "core")))))

(t/deftest read-mixed-legacy-and-dtcg-nodes
  (t/testing "the format is decided per token, not per file"
    (let [result (dtcg/read {"a" {"value" "1" "type" "number"}
                             "b" {"$value" "2" "$type" "number"}}
                            {:set-name "s"})]
      (t/is (= #{"a" "b"} (set (keys (tokens result "s"))))))))

;; Reading: DTCG 2025.10 values -----------------------------------------------

(t/deftest read-dimension-objects
  (let [result (dtcg/read {"space" {"$type" "dimension"
                                    "sm" {"$value" {"value" 4 "unit" "px"}}
                                    "md" {"$value" {"value" 0.5 "unit" "rem"}}}}
                          {:set-name "s"})]
    (t/is (= {"space.sm" {:value "4px" :type "dimension"}
              "space.md" {:value "0.5rem" :type "dimension"}}
             (tokens result "s")))))

(t/deftest read-color-objects
  (let [color (fn [v] (-> (dtcg/read {"c" {"$type" "color" "$value" v}} {:set-name "s"})
                          (tokens "s")
                          (get-in ["c" :value])))]
    (t/testing "hex is used when present"
      (t/is (= "#3366e6" (color {"colorSpace" "srgb" "components" [0.2 0.4 0.9] "hex" "#3366e6"}))))
    (t/testing "alpha is added to hex"
      (t/is (= "#00000080" (color {"colorSpace" "srgb" "components" [0 0 0] "alpha" 0.5 "hex" "#000000"}))))
    (t/testing "components are converted without hex"
      (t/is (= "#ff0000" (color {"colorSpace" "srgb" "components" [1 0 0]})))
      (t/is (= "#ff0000" (color {"colorSpace" "srgb-linear" "components" [1 0 0]})))
      (t/is (= "#ff0000" (color {"colorSpace" "oklch" "components" [0.628 0.2577 29.23]})))
      (t/is (= "#ff0000" (color {"colorSpace" "oklab" "components" [0.628 0.2249 0.1258]})))
      (t/is (= "#ff0000" (color {"colorSpace" "xyz-d65" "components" [0.4124 0.2126 0.0193]}))))
    (t/testing "HSL saturation and lightness are percentages"
      (t/is (= "#ff0000" (color {"colorSpace" "hsl" "components" [0 100 50]}))))
    (t/testing "none components are zero"
      (t/is (= "#000000" (color {"colorSpace" "srgb" "components" ["none" 0 0]})))))
  (t/testing "a color space without a conversion and without hex fails the token"
    (let [result (dtcg/read {"c" {"$type" "color"
                                  "$value" {"colorSpace" "lab" "components" [50 20 30]}}}
                            {:set-name "s"})]
      (t/is (empty? (tokens result "s")))
      (t/is (= [{:type :tokenscript/issue
                 :code :dtcg/invalid-value
                 :severity :error
                 :set "s"
                 :data {:reason :unsupported-color-space :color-space "lab" :token-name "c" :path []}}]
               (:issues result))))))

(t/deftest read-composite-values
  (let [result (dtcg/read
                {"type" {"$type" "typography"
                         "h1" {"$value" {"fontFamily" ["Helvetica" "Arial"]
                                         "fontSize" {"value" 42 "unit" "px"}
                                         "fontWeight" 700
                                         "lineHeight" 1.2}}}
                 "shadow" {"$type" "shadow"
                           "inner" {"$value" {"color" {"colorSpace" "srgb" "components" [0 0 0] "alpha" 0.5}
                                              "offsetX" {"value" 2 "unit" "px"}
                                              "offsetY" "{space.sm}"
                                              "blur" {"value" 4 "unit" "px"}
                                              "spread" {"value" 0 "unit" "px"}
                                              "inset" true}}
                           "legacy" {"$value" [{"x" "0" "y" "2px" "blur" "4px" "color" "#000"
                                                "type" "innerShadow"}]}}}
                {:set-name "s"})]
    (t/is (= {:value {"fontFamily" ["Helvetica" "Arial"] "fontSize" "42px" "fontWeight" 700 "lineHeight" 1.2}
              :type "typography"}
             (get (tokens result "s") "type.h1")))
    (t/is (= {:value [{"color" "#00000080" "offsetX" "2px" "offsetY" "{space.sm}" "blur" "4px" "spread" "0px" "inset" true}]
              :type "shadow"}
             (get (tokens result "s") "shadow.inner")))
    (t/is (= [{"offsetX" "0" "offsetY" "2px" "blur" "4px" "color" "#000" "inset" true}]
             (get-in (tokens result "s") ["shadow.legacy" :value])))))

(t/deftest read-spec-type-names
  (let [result (dtcg/read {"f" {"$type" "fontFamily" "$value" "Inter"}
                           "w" {"$type" "fontWeight" "$value" 700}}
                          {:set-name "s"})]
    (t/is (= "fontFamilies" (get-in (tokens result "s") ["f" :type])))
    (t/is (= "fontWeights" (get-in (tokens result "s") ["w" :type])))))

;; Reading: tree features -----------------------------------------------------

(t/deftest read-root-tokens
  (let [result (dtcg/read {"accent" {"$type" "color"
                                     "$root" {"$value" "#dd0000"}
                                     "light" {"$value" "#ff2222"}}}
                          {:set-name "s"})]
    (t/is (= {"accent" {:value "#dd0000" :type "color"}
              "accent.light" {:value "#ff2222" :type "color"}}
             (tokens result "s")))
    (t/testing "a token that is also a group is reported"
      (t/is (= {:dtcg/name-collision 1} (codes result))))))

(t/deftest read-extended-groups
  (let [result (dtcg/read {"button" {"$type" "color"
                                     "bg" {"$value" "#0066cc"}
                                     "text" {"$value" "#ffffff"}}
                           "button-primary" {"$extends" "{button}"
                                             "bg" {"$value" "#cc0066"}}}
                          {:set-name "s"})]
    (t/is (= {"button.bg" {:value "#0066cc" :type "color"}
              "button.text" {:value "#ffffff" :type "color"}
              "button-primary.bg" {:value "#cc0066" :type "color"}
              "button-primary.text" {:value "#ffffff" :type "color"}}
             (tokens result "s"))))
  (t/testing "circular extends"
    (let [result (dtcg/read {"a" {"$extends" "{b}" "x" {"$value" "1" "$type" "number"}}
                             "b" {"$extends" "{a}"}}
                            {:set-name "s"})]
      (t/is (contains? (codes result) :dtcg/circular-extends)))))

(t/deftest read-references-and-types
  (let [result (dtcg/read {"blue" {"$value" "#0066cc" "$type" "color"}
                           "primary" {"$value" "{blue}"}
                           "link" {"$value" "{primary}"}
                           "pointer" {"$ref" "#/blue/$value"}
                           "component" {"$ref" "#/blue/$value/components/0" "$type" "number"}
                           "untyped" {"$value" "8"}}
                          {:set-name "s"})]
    (t/testing "an alias takes the type of the token it references"
      (t/is (= {:value "{blue}" :type "color"} (get (tokens result "s") "primary")))
      (t/is (= {:value "{primary}" :type "color"} (get (tokens result "s") "link"))))
    (t/testing "a JSON pointer to a token is a reference"
      (t/is (= {:value "{blue}" :type "color"} (get (tokens result "s") "pointer"))))
    (t/testing "pointers into values and tokens without a type are dropped"
      (t/is (not (contains? (tokens result "s") "component")))
      (t/is (not (contains? (tokens result "s") "untyped")))
      (t/is (= {:dtcg/invalid-value 1 :dtcg/missing-type 1} (codes result))))))

(t/deftest read-properties-penpot-does-not-interpret
  (let [result (dtcg/read {"color" {"$description" "Colors"
                                    "$extensions" {"org.example" {"a" 1}}
                                    "$type" "color"
                                    "old" {"$value" "#000"
                                           "$deprecated" "Use new"
                                           "$extensions" {"com.figma" {"scopes" ["ALL"]}}
                                           "$custom" 1}}}
                          {:set-name "s"})
        set (first (:sets result))]
    (t/is (= {:value "#000" :type "color" :deprecated "Use new"
              :extensions {"com.figma" {"scopes" ["ALL"]}}
              :properties {"$custom" 1}}
             (get (:tokens set) "color.old")))
    (t/is (= {"color" {:description "Colors" :extensions {"org.example" {"a" 1}}}}
             (:groups set)))))

;; Reading: documents ---------------------------------------------------------

(t/deftest read-multi-set-documents
  (let [result (dtcg/read {"theme" {"a" {"$value" "1" "$type" "number"}}
                           "core" {"b" {"$value" "2" "$type" "number"}}
                           "light" {}
                           "$themes" [{"id" "t1" "name" "Light" "group" "Mode" "description" "Light mode"
                                       "isSource" false
                                       "selectedTokenSets" {"core" "source" "light" "enabled" "theme" "disabled"}}
                                      {"id" "t2" "name" "Dark"
                                       "selectedTokenSets" [{"id" "core" "status" "enabled"}]}]
                           "$metadata" {"tokenSetOrder" ["core" "light" "missing"]
                                        "activeThemes" ["Mode/Light"]
                                        "activeSets" {}}})]
    (t/is (= :multi-set (:document result)))
    (t/testing "sets follow tokenSetOrder, the others after it"
      (t/is (= ["core" "light" "theme"] (mapv :name (:sets result)))))
    (t/is (= [{:id "t1" :name "Light" :group "Mode" :description "Light mode" :is-source false
               :sets {"core" :source "light" :enabled "theme" :disabled}}
              {:id "t2" :name "Dark" :is-source false :sets {"core" :enabled}}]
             (:themes result)))
    (t/is (= ["Mode/Light"] (:active-themes result)))
    (t/testing "an empty object for a list of names is an empty list"
      (t/is (= [] (:active-sets result))))))

(t/deftest read-sets-without-metadata
  (let [result (dtcg/read {"core" {"a" {"$value" "1" "$type" "number"}}}
                          {:multi-set? true})]
    (t/is (= :multi-set (:document result)))
    (t/is (= ["core"] (mapv :name (:sets result))))))

(t/deftest read-invalid-documents
  (t/is (= :dtcg/invalid-document (th/error-code #(dtcg/read [1 2]))))
  (t/is (= :dtcg/invalid-document (th/error-code #(dtcg/read nil)))))

(t/deftest read-sets-resolve
  (t/testing "the sets of a 2025.10 file go into the processor as they are"
    (let [result (dtcg/read {"space" {"$type" "dimension"
                                      "base" {"$value" {"value" 8 "unit" "px"}}
                                      "double" {"$value" "{space.base} * 2"}}
                             "color" {"$type" "color"
                                      "brand" {"$value" {"colorSpace" "srgb" "components" [1 0 0]}}
                                      "alias" {"$value" "{color.brand}"}
                                      "dark" {"$value" "darken({color.brand}, 0.5)"}}}
                            {:set-name "s"})
          built (tsc/build (tokens result "s"))]
      (t/is (nil? (:issues built)))
      (t/is (= "16px" (get (processor/tokens->strings built) "space.double")))
      (t/is (= "#ff0000" (get (processor/tokens->strings built) "color.alias")))
      (t/is (some? (get (processor/tokens->strings built) "color.dark"))))))

;; Writing --------------------------------------------------------------------

(def ^:private document
  {:sets [{:name "core"
           :tokens {"space.sm" {:value "4px" :type "spacing" :description ""}
                    "color.brand" {:value "#ff000080" :type "color" :description "Brand"}
                    "font.body" {:value ["Inter" "sans-serif"] :type "fontFamilies" :description ""}
                    "type.body" {:value {"fontFamilies" ["Inter"] "fontSizes" "16px" "fontWeights" "700"
                                         "lineHeights" "1.5"}
                                 :type "typography"}
                    "shadow.sm" {:value [{"offsetX" "0" "offsetY" "2px" "blur" "4px" "spread" "0"
                                          "color" "#000" "inset" false}]
                                 :type "shadow"}
                    "ref" {:value "{space.sm} * 2" :type "spacing"}}}]
   :themes [{:id "t1" :name "Light" :group "Mode" :description "" :is-source false
             :sets {"core" :enabled}}]
   :active-themes #{"Mode/Light"}
   :active-sets #{"core"}})

(t/deftest write-tokens-studio-files
  (let [written (dtcg/write document)]
    (t/is (= {"$value" "4px" "$type" "spacing" "$description" ""}
             (get-in written ["core" "space" "sm"])))
    (t/is (= [{"id" "t1" "name" "Light" "group" "Mode" "description" "" "isSource" false
               "selectedTokenSets" {"core" "enabled"}}]
             (get written "$themes")))
    (t/is (= {"tokenSetOrder" ["core"] "activeThemes" #{"Mode/Light"} "activeSets" #{"core"}}
             (get written "$metadata")))
    (t/testing "reading it back gives the same document"
      (let [result (dtcg/read written)]
        (t/is (= (:tokens (first (:sets document))) (tokens result "core")))
        (t/is (= (:themes document) (:themes result)))))))

(t/deftest write-dtcg-2025-files
  (let [written (dtcg/write document {:profile :dtcg :original-type-namespace "com.example"})
        core (get written "core")]
    (t/is (= {"$value" {"value" 4 "unit" "px"} "$type" "dimension" "$description" ""
              "$extensions" {"com.example" {"type" "spacing"}}}
             (get-in core ["space" "sm"])))
    (t/is (= {"colorSpace" "srgb" "components" [1.0 0.0 0.0] "hex" "#ff0000" "alpha" 0.502}
             (get-in core ["color" "brand" "$value"])))
    (t/is (= "fontFamily" (get-in core ["font" "body" "$type"])))
    (t/is (= {"fontFamily" ["Inter"] "fontSize" {"value" 16 "unit" "px"} "fontWeight" 700 "lineHeight" 1.5}
             (get-in core ["type" "body" "$value"])))
    (t/is (= [{"offsetX" {"value" 0 "unit" "px"} "offsetY" {"value" 2 "unit" "px"}
               "blur" {"value" 4 "unit" "px"} "spread" {"value" 0 "unit" "px"}
               "color" {"colorSpace" "srgb" "components" [0.0 0.0 0.0] "hex" "#000000"}
               "inset" false}]
             (get-in core ["shadow" "sm" "$value"])))
    (t/testing "expressions stay strings"
      (t/is (= "{space.sm} * 2" (get-in core ["ref" "$value"]))))
    (t/testing "the type namespace restores the Tokens Studio types on read"
      (let [result (dtcg/read written {:original-type-namespace "com.example"})]
        (t/is (= {:value "4px" :type "spacing" :description ""}
                 (get (tokens result "core") "space.sm")))))))

(t/deftest write-root-tokens
  (let [written (dtcg/write {:sets [{:name "s" :tokens {"a" {:value "1" :type "number"}
                                                        "a.b" {:value "2" :type "number"}}}]
                             :themes []})]
    (t/is (= {"$root" {"$value" "1" "$type" "number"}
              "b" {"$value" "2" "$type" "number"}}
             (get-in written ["s" "a"])))
    (t/is (= {"a" {:value "1" :type "number"} "a.b" {:value "2" :type "number"}}
             (tokens (dtcg/read written) "s")))))

(t/deftest write-files
  (let [files (dtcg/write-files document)]
    (t/is (= #{"core.json" "$themes.json" "$metadata.json"} (set (keys files))))
    (t/is (= (get (dtcg/write document) "core") (get files "core.json")))))
