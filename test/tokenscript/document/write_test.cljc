;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL


(ns tokenscript.document.write-test
  (:require
   [clojure.test :as t]
   [tokenscript.document.read :as read]
   [tokenscript.document.write :as write]
   [tokenscript.test-helpers :as th]))

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

(t/deftest write-default-files
  (let [written (write/write document)]
    (t/is (= {"$value" "4px" "$type" "spacing" "$description" ""}
             (get-in written ["core" "space" "sm"])))
    (t/is (= [{"id" "t1" "name" "Light" "group" "Mode" "description" "" "isSource" false
               "selectedTokenSets" {"core" "enabled"}}]
             (get written "$themes")))
    (t/is (= {"tokenSetOrder" ["core"] "activeThemes" #{"Mode/Light"} "activeSets" #{"core"}}
             (get written "$metadata")))
    (t/testing "reading it back gives the same document"
      (let [result (read/read written)]
        (t/is (= (:tokens (first (:sets document))) (th/set-tokens result "core")))
        (t/is (= (:themes document) (:themes result)))
        (t/is (= ["Mode/Light"] (:active-themes result)))
        (t/is (= ["core"] (:active-sets result)))))))

(t/deftest write-dtcg-2025-files
  (let [written (write/write document {:profile :dtcg})
        core (get written "core")]
    (t/is (= {"$value" {"value" 4 "unit" "px"} "$type" "dimension" "$description" ""}
             (get-in core ["space" "sm"])))
    (t/is (= {"colorSpace" "srgb" "components" [1 0 0] "hex" "#ff0000" "alpha" (/ 128 255.0)}
             (get-in core ["color" "brand" "$value"])))
    (t/is (= "fontFamily" (get-in core ["font" "body" "$type"])))
    (t/is (= {"fontFamily" ["Inter"] "fontSize" {"value" 16 "unit" "px"} "fontWeight" 700 "lineHeight" 1.5}
             (get-in core ["type" "body" "$value"])))
    (t/is (= [{"offsetX" {"value" 0 "unit" "px"} "offsetY" {"value" 2 "unit" "px"}
               "blur" {"value" 4 "unit" "px"} "spread" {"value" 0 "unit" "px"}
               "color" {"colorSpace" "srgb" "components" [0 0 0] "hex" "#000000"}
               "inset" false}]
             (get-in core ["shadow" "sm" "$value"])))
    (t/testing "expressions stay strings"
      (t/is (= "{space.sm} * 2" (get-in core ["ref" "$value"]))))
    (t/testing "reading it back gives the DTCG types"
      (t/is (= {:value "4px" :type "dimension" :description ""}
               (get (th/set-tokens (read/read written) "core") "space.sm"))))))

(t/deftest write-colors
  (let [colors {"hex" {:value "#3366e6" :type "color"}
                "oklch" {:value "oklch(0.628, 0.2577, 29.23, 0.5)" :type "color"}
                "lab" {:value "lab(50, 20, 30)" :type "color"}
                "hsl" {:value "hsl(0, 100%, 50%)" :type "color"}
                "ref" {:value "{hex}" :type "color"}}
        written (get (write/write {:sets [{:name "s" :tokens colors}] :themes []} {:profile :dtcg}) "s")
        value #(get-in written [% "$value"])]
    (t/testing "hex colors are sRGB components without rounding, with the hex"
      (t/is (= {"colorSpace" "srgb" "components" [0.2 0.4 (/ 0xe6 255.0)] "hex" "#3366e6"}
               (value "hex"))))
    (t/testing "colors keep their color space, with the hex the schemas convert them to"
      (t/is (= {"colorSpace" "oklch" "components" [0.628 0.2577 29.23] "hex" "#ff0000" "alpha" 0.5}
               (value "oklch")))
      (t/is (= {"colorSpace" "lab" "components" [50 20 30]} (value "lab")) "no conversion to hex"))
    (t/testing "HSL is written in sRGB: DTCG has its saturation and lightness as percentages"
      (t/is (= {"colorSpace" "srgb" "components" [1 0 0] "hex" "#ff0000"} (value "hsl"))))
    (t/is (= "{hex}" (value "ref")))
    (t/testing "reading them back gives the same colors"
      (t/is (= (assoc colors "hsl" {:value "#ff0000" :type "color"})
               (th/set-tokens (read/read {"s" written} {:multi-set? true}) "s"))))))

(t/deftest write-root-tokens
  (let [written (write/write {:sets [{:name "s" :tokens {"a" {:value "1" :type "number"}
                                                         "a.b" {:value "2" :type "number"}}}]
                              :themes []})]
    (t/is (= {"$root" {"$value" "1" "$type" "number"}
              "b" {"$value" "2" "$type" "number"}}
             (get-in written ["s" "a"])))
    (t/testing "a document without active themes and sets writes empty lists"
      (t/is (= {"tokenSetOrder" ["s"] "activeThemes" [] "activeSets" []}
               (get written "$metadata"))))
    (t/is (= {"a" {:value "1" :type "number"} "a.b" {:value "2" :type "number"}}
             (th/set-tokens (read/read written) "s")))))

(t/deftest write-files
  (let [files (write/write-files document)]
    (t/is (= #{"core.json" "$themes.json" "$metadata.json"} (set (keys files))))
    (t/is (= (get (write/write document) "core") (get files "core.json")))))
