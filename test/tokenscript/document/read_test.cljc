;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns tokenscript.document.read-test
  (:require
   [clojure.test :as t]
   [tokenscript.core :as tsc]
   [tokenscript.document.read :as read]
   [tokenscript.processor :as processor]
   [tokenscript.symbols :as sym]
   [tokenscript.test-helpers :as th]))

(defn- codes
  [result]
  (frequencies (map :code (:issues result))))

;; Reading: formats -----------------------------------------------------------

(t/deftest read-legacy-tokens
  (let [result (read/read {"color" {"red" {"value" "#ff0000" "type" "color"
                                           "description" "Red"}}
                           "space" {"sm" {"value" "4" "type" "spacing"}}}
                          {:set-name "core"})]
    (t/is (= :single-set (:document result)))
    (t/is (= {"color.red" {:value "#ff0000" :type "color" :description "Red"}
              "space.sm" {:value "4" :type "spacing"}}
             (th/set-tokens result "core")))
    (t/is (empty? (:issues result)))))

(t/deftest read-draft-tokens-with-group-types
  (let [result (read/read {"colors" {"$type" "color"
                                     "red" {"$value" "#ff0000"}
                                     "brand" {"$value" "#0000ff" "$description" "Brand"}
                                     "label" {"$value" "Red" "$type" "string"}}}
                          {:set-name "core"})]
    (t/is (= {"colors.red" {:value "#ff0000" :type "color"}
              "colors.brand" {:value "#0000ff" :type "color" :description "Brand"}
              "colors.label" {:value "Red" :type "string"}}
             (th/set-tokens result "core")))))

(t/deftest read-mixed-legacy-and-dtcg-nodes
  (t/testing "the format is decided per token, not per file"
    (let [result (read/read {"a" {"value" "1" "type" "number"}
                             "b" {"$value" "2" "$type" "number"}}
                            {:set-name "s"})]
      (t/is (= #{"a" "b"} (set (keys (th/set-tokens result "s"))))))))

(t/deftest read-groups-with-tokens-named-value-and-type
  (t/testing "a group whose tokens are named value and type is not a legacy token"
    (let [result (read/read {"g" {"value" {"$value" "1" "$type" "number"}
                                  "type" {"$value" "2" "$type" "number"}}
                             "h" {"value" {"value" "3" "type" "number"}
                                  "type" {"value" "4" "type" "number"}}}
                            {:set-name "s"})]
      (t/is (= {"g.value" {:value "1" :type "number"}
                "g.type" {:value "2" :type "number"}
                "h.value" {:value "3" :type "number"}
                "h.type" {:value "4" :type "number"}}
               (th/set-tokens result "s")))
      (t/is (empty? (:issues result))))))

(t/deftest read-tokens-named-value-and-type
  (t/testing "a group with tokens named value and type is not a legacy token"
    (let [result (read/read {"line" {"$type" "number"
                                     "value" {"$value" "1"}
                                     "type" {"$value" "2"}}
                             "legacy" {"value" {"value" "3" "type" "number"}
                                       "type" {"value" "4" "type" "number"}}}
                            {:set-name "s"})]
      (t/is (= {"line.value" {:value "1" :type "number"}
                "line.type" {:value "2" :type "number"}
                "legacy.value" {:value "3" :type "number"}
                "legacy.type" {:value "4" :type "number"}}
               (th/set-tokens result "s"))))))

;; Reading: DTCG 2025.10 values -----------------------------------------------

(t/deftest read-dimension-objects
  (let [result (read/read {"space" {"$type" "dimension"
                                    "sm" {"$value" {"value" 4 "unit" "px"}}
                                    "md" {"$value" {"value" 0.5 "unit" "rem"}}}}
                          {:set-name "s"})]
    (t/is (= {"space.sm" {:value "4px" :type "dimension"}
              "space.md" {:value "0.5rem" :type "dimension"}}
             (th/set-tokens result "s")))))

(t/deftest read-duration-objects
  (t/testing "durations have the shape of dimensions (DTCG 2025.10, 8.5)"
    (let [result (read/read {"motion" {"$type" "duration"
                                       "quick" {"$value" {"value" 100 "unit" "ms"}}
                                       "long" {"$value" {"value" 1.5 "unit" "s"}}}}
                            {:set-name "s"})]
      (t/is (= {"motion.quick" {:value "100ms" :type "duration"}
                "motion.long" {:value "1.5s" :type "duration"}}
               (th/set-tokens result "s")))
      (t/is (empty? (:issues result)))))
  (t/testing "durations in a transition (DTCG 2025.10, 9.5)"
    (let [result (read/read {"emphasis" {"$type" "transition"
                                         "$value" {"duration" {"value" 200 "unit" "ms"}
                                                   "delay" {"value" 0 "unit" "ms"}
                                                   "timingFunction" [0.5 0 1 1]}}}
                            {:set-name "s"})]
      (t/is (= {"duration" "200ms" "delay" "0ms" "timingFunction" [0.5 0 1 1]}
               (get-in (th/set-tokens result "s") ["emphasis" :value]))))))

(defn- read-color
  "The value and the issues of a color token."
  [v]
  (let [result (read/read {"c" {"$type" "color" "$value" v}} {:set-name "s"})]
    [(get-in (th/set-tokens result "s") ["c" :value])
     (mapv (juxt :code :severity :data) (:issues result))]))

(t/deftest read-color-objects
  (let [color (comp first read-color)]
    (t/testing "colors keep their color space"
      (t/is (= "srgb(1, 0, 0)" (color {"colorSpace" "srgb" "components" [1 0 0]})))
      (t/is (= "linearsrgb(1, 0, 0)" (color {"colorSpace" "srgb-linear" "components" [1 0 0]})))
      (t/is (= "oklch(0.628, 0.2577, 29.23)" (color {"colorSpace" "oklch" "components" [0.628 0.2577 29.23]})))
      (t/is (= "oklab(0.628, 0.2249, 0.1258)" (color {"colorSpace" "oklab" "components" [0.628 0.2249 0.1258]})))
      (t/is (= "xyzd65(0.4124, 0.2126, 0.0193)" (color {"colorSpace" "xyz-d65" "components" [0.4124 0.2126 0.0193]})))
      (t/is (= "xyzd50(0.4, 0.2, 0.02)" (color {"colorSpace" "xyz-d50" "components" [0.4 0.2 0.02]})))
      (t/is (= "lab(50, 20, 30)" (color {"colorSpace" "lab" "components" [50 20 30]})))
      (t/is (= "lch(50, 36, 56)" (color {"colorSpace" "lch" "components" [50 36 56]})))
      (t/is (= "p3(1, 0, 0)" (color {"colorSpace" "display-p3" "components" [1 0 0]}))))
    (t/testing "alpha is the fourth argument"
      (t/is (= "oklch(0.628, 0.2577, 29.23, 0.5)"
               (color {"colorSpace" "oklch" "components" [0.628 0.2577 29.23] "alpha" 0.5})))
      (t/is (= "linearsrgb(1, 0, 0, 0.5)" (color {"colorSpace" "srgb-linear" "components" [1 0 0] "alpha" 0.5})))
      (t/is (= "srgb(1, 0, 0)" (color {"colorSpace" "srgb" "components" [1 0 0] "alpha" 1})) "opaque"))
    (t/testing "HSL saturation and lightness are percentages"
      (t/is (= "hsl(0, 100%, 50%)" (color {"colorSpace" "hsl" "components" [0 100 50]}))))
    (t/testing "none components are zero"
      (t/is (= "srgb(0, 0, 0)" (color {"colorSpace" "srgb" "components" ["none" 0 0]}))))
    (t/testing "hex is used when it is exactly the sRGB color of the components"
      (t/is (= "srgb(0.2, 0.4, 0.9)" (color {"colorSpace" "srgb" "components" [0.2 0.4 0.9] "hex" "#3366e6"})))
      (t/is (= "#3366e6" (color {"colorSpace" "srgb" "components" [0.2 0.4 (/ 0xe6 255.0)] "hex" "#3366e6"})))
      (t/is (= "#00000080" (color {"colorSpace" "srgb" "components" [0 0 0] "alpha" 0.5 "hex" "#000000"})))
      (t/is (= "lab(50, 20, 30)" (color {"colorSpace" "lab" "components" [50 20 30] "hex" "#a46a4f"}))
            "other color spaces keep their components")))
  (t/testing "the colors resolve to the color of the file"
    (let [built (tsc/build {"oklch" {:value "oklch(0.628, 0.2577, 29.23, 0.5)" :type "color"}
                            "hsl" {:value "hsl(0, 100%, 50%)" :type "color"}
                            "linear" {:value "linearsrgb(1, 0, 0, 0.5)" :type "color"}})]
      (t/is (nil? (:issues built)))
      (t/is (= {"oklch" "#ff000080" "hsl" "#ff0000" "linear" "#ff000080"}
               (update-vals (:tokens built) #(sym/to-string (tsc/color->hex %)))))))
  (t/testing "a color space the schemas don't have reads the hex with a warning"
    (t/is (= ["#ff0000" [[:dtcg/unsupported-color-space :warning
                          {:color-space "rec2020" :token-name "c" :path []}]]]
             (read-color {"colorSpace" "rec2020" "components" [1 0 0] "hex" "#ff0000"}))))
  (t/testing "a color space the schemas don't have fails the token without hex"
    (t/is (= [nil [[:dtcg/invalid-value :error
                    {:reason :unsupported-color-space :color-space "rec2020" :token-name "c" :path []}]]]
             (read-color {"colorSpace" "rec2020" "components" [1 0 0]})))
    (t/is (= [nil [[:dtcg/invalid-value :error
                    {:reason :unsupported-color-space :color-space "hwb" :token-name "c" :path []}]]]
             (read-color {"colorSpace" "hwb" "components" [0 20 30]}))
          "the HWB schema reads whiteness and blackness as 0-1, DTCG as percentages")))

(t/deftest read-composite-values
  (let [result (read/read
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
             (get (th/set-tokens result "s") "type.h1")))
    (t/is (= {:value [{"color" "srgb(0, 0, 0, 0.5)" "offsetX" "2px" "offsetY" "{space.sm}" "blur" "4px" "spread" "0px" "inset" true}]
              :type "shadow"}
             (get (th/set-tokens result "s") "shadow.inner")))
    (t/is (= [{"offsetX" "0" "offsetY" "2px" "blur" "4px" "color" "#000" "inset" true}]
             (get-in (th/set-tokens result "s") ["shadow.legacy" :value])))))

(t/deftest read-spec-type-names
  (let [result (read/read {"f" {"$type" "fontFamily" "$value" "Inter"}
                           "w" {"$type" "fontWeight" "$value" 700}}
                          {:set-name "s"})]
    (t/is (= "fontFamilies" (get-in (th/set-tokens result "s") ["f" :type])))
    (t/is (= "fontWeights" (get-in (th/set-tokens result "s") ["w" :type])))))

;; Reading: tree features -----------------------------------------------------

(t/deftest read-root-tokens
  (let [result (read/read {"accent" {"$type" "color"
                                     "$root" {"$value" "#dd0000"}
                                     "light" {"$value" "#ff2222"}}}
                          {:set-name "s"})]
    (t/is (= {"accent" {:value "#dd0000" :type "color"}
              "accent.light" {:value "#ff2222" :type "color"}}
             (th/set-tokens result "s")))
    (t/testing "names below another token are left to the resolver"
      (t/is (empty? (:issues result))))
    (t/testing "$root is not a property of its group"
      (t/is (= {} (:groups (first (:sets result))))))))

(t/deftest read-extended-groups
  (let [result (read/read {"button" {"$type" "color"
                                     "bg" {"$value" "#0066cc"}
                                     "text" {"$value" "#ffffff"}}
                           "button-primary" {"$extends" "{button}"
                                             "bg" {"$value" "#cc0066"}}}
                          {:set-name "s"})]
    (t/is (= {"button.bg" {:value "#0066cc" :type "color"}
              "button.text" {:value "#ffffff" :type "color"}
              "button-primary.bg" {:value "#cc0066" :type "color"}
              "button-primary.text" {:value "#ffffff" :type "color"}}
             (th/set-tokens result "s"))))
  (t/testing "nested groups are merged, tokens are replaced whole (DTCG 2025.10, 6.4.3)"
    (let [result (read/read {"input" {"field" {"$type" "dimension"
                                               "width" {"$value" "12rem" "$description" "Base"}
                                               "height" {"$value" "2rem"}}}
                             "input-amount" {"$extends" "{input}"
                                             "field" {"width" {"$value" "100px"}}}}
                            {:set-name "s"})]
      (t/is (= {"input-amount.field.width" {:value "100px" :type "dimension"}
                "input-amount.field.height" {:value "2rem" :type "dimension"}}
               (select-keys (th/set-tokens result "s") ["input-amount.field.width" "input-amount.field.height"])))
      (t/is (empty? (:issues result)))))
  (t/testing "extends follows chains and names nested groups"
    (let [result (read/read {"a" {"$type" "number" "x" {"$value" "1"}}
                             "b" {"$extends" "{a}" "y" {"$value" "2"}}
                             "c" {"$extends" "{b}" "z" {"$value" "3"}}
                             "base" {"inner" {"$type" "number" "w" {"$value" "4"}}}
                             "d" {"$extends" "{base.inner}"}}
                            {:set-name "s"})]
      (t/is (= {"c.x" {:value "1" :type "number"}
                "c.y" {:value "2" :type "number"}
                "c.z" {:value "3" :type "number"}
                "d.w" {:value "4" :type "number"}}
               (select-keys (th/set-tokens result "s") ["c.x" "c.y" "c.z" "d.w"])))
      (t/is (empty? (:issues result)))))
  (t/testing "circular extends"
    (let [result (read/read {"a" {"$extends" "{b}" "x" {"$value" "1" "$type" "number"}}
                             "b" {"$extends" "{a}"}}
                            {:set-name "s"})]
      (t/is (contains? (codes result) :dtcg/circular-extends))))
  (t/testing "a group extending its own parent is circular (DTCG 2025.10, 6.4.4)"
    (let [result (read/read {"button" {"color" {"$type" "color" "$value" "#000"}
                                       "secondary" {"$extends" "{button}"}}}
                            {:set-name "s"})]
      (t/is (= {:dtcg/circular-extends 1} (codes result)))
      (t/is (= {"button.color" {:value "#000" :type "color"}} (th/set-tokens result "s")))))
  (t/testing "extends that is not a reference, names a token or names nothing"
    (let [result (read/read {"t" {"$value" "1" "$type" "number"}
                             "a" {"$extends" "button"}
                             "b" {"$extends" "{t}"}
                             "c" {"$extends" "{missing}"}}
                            {:set-name "s"})]
      (t/is (= [{:group "a" :extends "button"} {:group "b" :extends "{t}"} {:group "c" :extends "{missing}"}]
               (->> (:issues result)
                    (filter #(= :dtcg/invalid-extends (:code %)))
                    (map :data)
                    (sort-by :group))))
      (t/is (= {"t" {:value "1" :type "number"}} (th/set-tokens result "s"))))))

(t/deftest read-group-refs
  (t/testing "a $ref of a group extends the group it points at (DTCG 2025.10, 6.4.1)"
    (let [result (read/read {"button" {"$type" "color"
                                       "bg" {"$value" "#0066cc"}
                                       "text" {"$value" "#ffffff"}}
                             "button-primary" {"$ref" "#/button"
                                               "bg" {"$value" "#cc0066"}}
                             "button-copy" {"$ref" "#/button"}}
                            {:set-name "s"})]
      (t/is (= {"button.bg" {:value "#0066cc" :type "color"}
                "button.text" {:value "#ffffff" :type "color"}
                "button-primary.bg" {:value "#cc0066" :type "color"}
                "button-primary.text" {:value "#ffffff" :type "color"}
                "button-copy.bg" {:value "#0066cc" :type "color"}
                "button-copy.text" {:value "#ffffff" :type "color"}}
               (th/set-tokens result "s")))
      (t/is (empty? (:issues result)))))
  (t/testing "a $ref on a group with tokens of its own cannot point at a token"
    (let [result (read/read {"blue" {"$value" "#0066cc" "$type" "color"}
                             "group" {"$ref" "#/blue"
                                      "x" {"$value" "#ffffff" "$type" "color"}}}
                            {:set-name "s"})]
      (t/is (= {:dtcg/invalid-extends 1} (codes result)))
      (t/is (= #{"blue" "group.x"} (set (keys (th/set-tokens result "s"))))))))

(t/deftest read-references-and-types
  (let [result (read/read {"blue" {"$value" "#0066cc" "$type" "color"}
                           "primary" {"$value" "{blue}"}
                           "link" {"$value" "{primary}"}
                           "pointer" {"$ref" "#/blue/$value"}
                           "token-pointer" {"$ref" "#/blue"}
                           "component" {"$ref" "#/blue/$value/components/0" "$type" "number"}
                           "untyped" {"$value" "8"}}
                          {:set-name "s"})]
    (t/testing "an alias takes the type of the token it references"
      (t/is (= {:value "{blue}" :type "color"} (get (th/set-tokens result "s") "primary")))
      (t/is (= {:value "{primary}" :type "color"} (get (th/set-tokens result "s") "link"))))
    (t/testing "a JSON pointer to a token or its value is a reference"
      (t/is (= {:value "{blue}" :type "color"} (get (th/set-tokens result "s") "pointer")))
      (t/is (= {:value "{blue}" :type "color"} (get (th/set-tokens result "s") "token-pointer"))))
    (t/testing "pointers into values and tokens without a type are dropped"
      (t/is (not (contains? (th/set-tokens result "s") "component")))
      (t/is (not (contains? (th/set-tokens result "s") "untyped")))
      (t/is (= {:dtcg/invalid-value 1 :dtcg/missing-type 1} (codes result))))))

(t/deftest read-pointers-in-values
  (let [result (read/read {"base" {"$type" "typography"
                                   "font" {"$value" "Inter" "$type" "fontFamily"}
                                   "text" {"$value" {"fontFamily" {"$ref" "#/base/font/$value"}
                                                     "fontSize" {"value" 16 "unit" "px"}}}
                                   "h1" {"$value" {"fontFamily" "Inter"
                                                   "fontSize" {"value" {"$ref" "#/base/text/$value/fontSize/value"}
                                                               "unit" "rem"}}}}}
                          {:set-name "s"})]
    (t/testing "a pointer to a token's value is a reference"
      (t/is (= {"fontFamily" "{base.font}" "fontSize" "16px"}
               (get-in (th/set-tokens result "s") ["base.text" :value]))))
    (t/testing "a pointer into a value fails the token"
      (t/is (not (contains? (th/set-tokens result "s") "base.h1")))
      (t/is (= [{:reason :unsupported-pointer
                 :pointer "#/base/text/$value/fontSize/value"
                 :path ["fontSize" "value"]
                 :token-name "base.h1"}]
               (map :data (:issues result)))))))

(t/deftest read-properties-penpot-does-not-interpret
  (let [result (read/read {"color" {"$description" "Colors"
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
  (let [result (read/read {"theme" {"a" {"$value" "1" "$type" "number"}}
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
  (let [result (read/read {"core" {"a" {"$value" "1" "$type" "number"}}}
                          {:multi-set? true})]
    (t/is (= :multi-set (:document result)))
    (t/is (= ["core"] (mapv :name (:sets result))))))

(t/deftest read-invalid-documents
  (t/is (= :dtcg/invalid-document (th/error-code #(read/read [1 2]))))
  (t/is (= :dtcg/invalid-document (th/error-code #(read/read nil)))))

(t/deftest read-sets-resolve
  (t/testing "the sets of a 2025.10 file go into the processor as they are"
    (let [result (read/read {"space" {"$type" "dimension"
                                      "base" {"$value" {"value" 8 "unit" "px"}}
                                      "double" {"$value" "{space.base} * 2"}}
                             "color" {"$type" "color"
                                      "brand" {"$value" {"colorSpace" "srgb" "components" [1 0 0]}}
                                      "alias" {"$value" "{color.brand}"}
                                      "dark" {"$value" "darken({color.brand}, 0.5)"}}}
                            {:set-name "s"})
          built (tsc/build (th/set-tokens result "s"))]
      (t/is (nil? (:issues built)))
      (t/is (= "16px" (get (processor/tokens->strings built) "space.double")))
      (t/is (= "srgb(1, 0, 0)" (get (processor/tokens->strings built) "color.alias")))
      (t/is (some? (get (processor/tokens->strings built) "color.dark"))))))
