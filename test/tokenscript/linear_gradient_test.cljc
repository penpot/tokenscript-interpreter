;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns tokenscript.linear-gradient-test
  "Tests the CSS `linear-gradient()` of the bundle. The bundle defines it
  as a tokenscript schema function."
  (:require
   [clojure.string :as str]
   [clojure.test :as t]
   [tokenscript.core :as tsc]
   [tokenscript.errors :as err]
   [tokenscript.importers.css :as css]
   [tokenscript.processor :as proc]
   [tokenscript.symbols :as sym]))

(def ^:private cfg
  (delay (tsc/default-config)))

(defn- whole
  "Gives whole numbers as integers, because the JVM and ClojureScript
  differ here."
  [n]
  #?(:clj (if (== n (long n)) (long n) n)
     :cljs n))

(defn- plain
  "Converts the symbol to Clojure data, as a consumer reads the resolved
  value of a token. Colors become hex. It drops the units, and a
  percentage becomes a fraction."
  [v]
  (cond
    (sym/null? v)       :null
    (sym/dictionary? v) (into {} (map (fn [[k x]] [k (plain x)])) (sym/value v))
    (sym/list? v)       (mapv plain (sym/value v))
    (sym/color? v)      (str/lower-case (sym/to-string (tsc/color->hex @cfg v) @cfg))
    (sym/number-with-unit? v) (whole (if (= "%" (:unit v)) (/ (sym/value v) 100) (sym/value v)))
    (number? (sym/value v)) (whole (sym/value v))
    :else               (sym/value v)))

(defn- gradient
  "Resolves `value` as a gradient token, with `{c}` as `#ff0000` and
  `{p}` as `20%`. Gives the gradient or the error code."
  [value]
  (let [res (proc/build {"c" {:value "#ff0000"}
                         "p" {:value "20%"}
                         "t" {:value value :type "gradient"}}
                        {:config @cfg})
        v   (get-in res [:resolved "t"])]
    (if (err/error-map? v)
      (:code v)
      (plain v))))

(t/deftest linear-gradient-test
  (t/testing "the gradient token value"
    (t/is (= {"kind" "linear"
              "angle" 90
              "stops" [{"color" "#5500ff" "position" 0}
                       {"color" "#00ff55" "position" 1}]}
             (gradient "linear-gradient(to right, #5500ff, #00ff55)"))))

  (t/testing "the direction"
    (t/are [direction angle] (= angle (get (gradient (str "linear-gradient(" direction ", #f00, #00f)")) "angle"))
      "#f00"          180
      "to top"        0
      "to bottom"     180
      "to left"       270
      "to right top"  45
      "to bottom right" 135
      "to left bottom" 225
      "to top left"   315
      "-45deg"        -45
      "45deg"         45
      "0.25turn"      90
      "-0.5turn"      -180
      "100grad"       90
      "1rad"          (/ 180 #?(:clj Math/PI :cljs js/Math.PI))))

  (t/testing "stops are spread as CSS does"
    (t/is (= [0 0.1 0.4 0.7 1]
             (map #(get % "position")
                  (get (gradient "linear-gradient(#f00, #0f0 10%, #ff0, #00f, #f0f)") "stops")))))

  (t/testing "a color with two positions is two stops, a color hint is left out"
    (t/is (= [["#ff0000" 0] ["#ff0000" 0.5] ["#0000ff" 0.5] ["#0000ff" 1]]
             (map (juxt #(get % "color") #(get % "position"))
                  (get (gradient "linear-gradient({c} 0% 50%, #0000ff 50% 100%)") "stops"))))
    (t/is (= 2 (count (get (gradient "linear-gradient(#f00, 30%, #00f)") "stops")))))

  (t/testing "positions are clamped to the gradient"
    (t/is (= [0 1]
             (map #(get % "position")
                  (get (gradient "linear-gradient(#f00 0%, #00f 150%)") "stops")))))

  (t/testing "colors with an alpha, and references"
    (t/is (= ["#ffd16699" "#ff0000"]
             (map #(get % "color")
                  (get (gradient "linear-gradient(rgba(255, 209, 102, 0.6), {c})") "stops"))))
    (t/is (= [0.2 1]
             (map #(get % "position")
                  (get (gradient "linear-gradient(90deg, {c} {p}, #00f 100%)") "stops")))))

  (t/testing "arguments that make no gradient"
    (t/is (= :functions/requires-min-arguments (gradient "linear-gradient(#f00)")))
    (t/is (= :null (gradient "linear-gradient(to nowhere, #f00, #00f)")))
    (t/is (= :null (gradient "linear-gradient(#f00, 8)")))
    (t/is (= :null (gradient "linear-gradient(#f00 3px, #00f)")))))

(t/deftest imported-css-test
  (t/testing "the tokenscript of CSS evaluates to the gradient the CSS is"
    (doseq [text ["linear-gradient(to right, #5500ff, #00ff55)"
                  "linear-gradient(135deg, #f00 0%, #00f 100%)"
                  "linear-gradient(90deg, rgba(255, 0, 0, 0.5) 25%, #00ff55)"
                  "linear-gradient(0.25turn, hsl(120, 1, 0.5), rgb(0, 0, 255))"
                  "linear-gradient(#f00 0% 50%, #00f 50% 100%)"
                  "linear-gradient(45deg, {c}, {c} 40%, #0000ff)"
                  "linear-gradient(red -20%, #0000ff 150%)"]]
      (t/is (= (gradient (css/linear-gradient->tokenscript text))
               (gradient (str/replace text "red -20%" "#ff0000 0%")))
            text)))

  (t/testing "CSS color syntax tokenscript does not read becomes one it does"
    (t/is (= {"kind" "linear"
              "angle" 90
              "stops" [{"color" "#ff000080" "position" 0}
                       {"color" "#00ff55" "position" 1}]}
             (gradient (css/linear-gradient->tokenscript
                        "linear-gradient(to right, rgb(255 0 0 / 50%), #00ff55)"))))))