;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns tokenscript.css-functions-test
  "The CSS color functions of the bundle (`rgb`, `rgba`, `hsl`, `hsla`),
  written as tokenscript schema functions, and their native
  implementations (`tokenscript.css-colors`), checked against the
  scripts."
  (:require
   [clojure.test :as t]
   [clojure.test.check :as tc]
   [clojure.test.check.generators :as gen]
   [clojure.test.check.properties :as prop]
   [tokenscript.config :as config]
   [tokenscript.core :as tsc]
   [tokenscript.css-colors :as css-colors]
   [tokenscript.errors :as err]
   [tokenscript.functions :as functions]
   [tokenscript.interpreter :as interp]
   [tokenscript.processor :as proc]
   [tokenscript.schemas :as schemas]
   [tokenscript.symbols :as sym]
   [tokenscript.test-helpers :as h]))

(defn- resolve-hex
  "Hex string of the color `value` resolves to (with `{color}` as
  `#ff0000` and `{hue}` as 120), or the error code."
  [value]
  (let [cfg (h/full-config)
        res (proc/build {"color" {:value "#ff0000"}
                         "hue" {:value "120"}
                         "t" {:value value}}
                        {:config cfg})
        v (get-in res [:resolved "t"])]
    (cond
      (err/error-map? v) (:code v)
      (sym/color? v) (sym/to-string (tsc/color->hex cfg v) cfg)
      :else v)))

(t/deftest rgb-test
  (t/testing "comma and space separated channels"
    (t/is (= "#ff0000" (resolve-hex "rgb(255, 0, 0)")))
    (t/is (= "#f4fbfc" (resolve-hex "rgb(244 251 252)")))
    (t/is (= "#ff0000" (resolve-hex "RGB(255, 0, 0)"))))
  (t/testing "percentages"
    (t/is (= "#ff0000" (resolve-hex "rgb(100%, 0%, 0%)")))
    (t/is (= "#808080" (resolve-hex "rgb(50%, 50%, 50%)"))))
  (t/testing "alpha as number or percentage, with rgb and rgba"
    (t/is (= "#ff000080" (resolve-hex "rgba(255, 0, 0, 0.5)")))
    (t/is (= "#ff000080" (resolve-hex "rgba(255, 0, 0, 50%)")))
    (t/is (= "#ff000080" (resolve-hex "rgba(255 0 0 50%)")))
    (t/is (= "#ff000080" (resolve-hex "rgb(255, 0, 0, 0.5)"))))
  (t/testing "a color and an alpha"
    (t/is (= "#ff000080" (resolve-hex "rgba({color}, 0.5)")))
    (t/is (= "#ff000080" (resolve-hex "rgba({color}, 50%)")))
    (t/is (= "#ff000033" (resolve-hex "rgba(red, 0.2)")))
    (t/is (= "#ff0000" (resolve-hex "rgb({color})"))))
  (t/testing "channels are clamped and rounded like CSS"
    (t/is (= "#ff0000" (resolve-hex "rgb(255.5, 0, 0)")))
    (t/is (= "#ff0000" (resolve-hex "rgb(300, -5, 0)")))
    (t/is (= "#ff0000" (resolve-hex "rgba(255, 0, 0, 2)"))))
  (t/testing "an alpha of 1 is opaque, also over a color with alpha"
    (t/is (= "#ff0000" (resolve-hex "rgba(255, 0, 0, 1)")))
    (t/is (= "#ff0000" (resolve-hex "rgba(rgba({color}, 0.5), 1)"))))
  (t/testing "invalid arguments are errors"
    (t/is (= :functions/execution-error (resolve-hex "rgb(1, 2)")))
    (t/is (= :functions/execution-error (resolve-hex "rgb(a, b, c)")))
    (t/is (= :functions/execution-error (resolve-hex "rgb(10px, 0, 0)")))))

(t/deftest hsl-test
  (t/testing "CSS syntaxes"
    (t/is (= "#ff0000" (resolve-hex "hsl(0, 100%, 50%)")))
    (t/is (= "#ff0000" (resolve-hex "hsl(0 100% 50%)")))
    (t/is (= "#00ff00" (resolve-hex "hsl(120deg, 100%, 50%)")))
    (t/is (= "#00ff00" (resolve-hex "hsl(120deg 100% 50%)")))
    (t/is (= "#40bf40" (resolve-hex "hsl({hue}, 50%, 50%)"))))
  (t/testing "the hue wraps around"
    (t/is (= "#0000ff" (resolve-hex "hsl(-120, 100%, 50%)")))
    (t/is (= "#00ff00" (resolve-hex "hsl(480, 100%, 50%)"))))
  (t/testing "plain numbers are fractions, or CSS percentages when above 1"
    (t/is (= "#ff0000" (resolve-hex "hsl(0, 1, 0.5)")))
    (t/is (= "#0000ff" (resolve-hex "hsl(240, 1, 0.5)")))
    (t/is (= "#ff0000" (resolve-hex "hsl(0, 100, 50)")))
    (t/is (= "#40bf40" (resolve-hex "hsl(120, 50, 50)")))
    (t/is (= "#ff0000" (resolve-hex "hsl(0, 100%, 0.5)"))))
  (t/testing "alpha"
    (t/is (= "#ff000080" (resolve-hex "hsla(0, 100%, 50%, 0.5)")))
    (t/is (= "#ff000080" (resolve-hex "hsla(0, 100%, 50%, 50%)")))
    (t/is (= "#ff000080" (resolve-hex "hsla(0 100% 50% 50%)")))
    (t/is (= "#ff000080" (resolve-hex "hsla({color}, 0.5)"))))
  (t/testing "out of range saturation is clamped"
    (t/is (= "#ff0000" (resolve-hex "hsl(0, 150%, 50%)"))))
  (t/testing "invalid arguments are errors"
    (t/is (= :functions/execution-error (resolve-hex "hsl(0, 100%)")))
    (t/is (= :functions/execution-error (resolve-hex "hsl(a, b, c)")))))

(t/deftest color-methods-still-convert-test
  (t/testing "the functions shadow the rgb/hsl initializers, not the conversions"
    (let [opts {:config (h/full-config)}]
      (t/is (= "#ff0000" (h/ev "return #ff0000.to.rgb().to.hex();" nil opts)))
      (t/is (= "#00ff00" (h/ev "return hsl(120, 1, 0.5).to.hex();" nil opts)))
      (t/is (= "#00ff00" (h/ev "variable c: Color.HSL = hsl(120deg, 100%, 50%); return c.to.hex();" nil opts))))))
;; Native implementations ------------------------------------------------------
;;
;; The scripts are the reference: every case runs under `h/full-config`
;; (native) and `h/script-config` (scripts) and must print the same
;; result symbol or error map.

(def ^:private color-functions ["rgb" "rgba" "hsl" "hsla"])

(defn- printed
  "`thunk`'s result or raised error map, printed: `pr-str` tells apart
  values that `=` does not (`0` and `0.0`, `NaN`)."
  [thunk]
  (pr-str (try
            (thunk)
            (catch #?(:clj Throwable :cljs :default) e
              (or (err/->error-map e) (throw e))))))

(defn- same-as-script?
  [run]
  (= (printed #(run (h/full-config))) (printed #(run (h/script-config)))))

(def ^:private colors
  "Colors by name, built in the full config."
  (delay
    (let [cfg (h/full-config)
          red (sym/color "#ff0000" "Hex" nil)]
      {:hex red
       :hex-alpha (sym/color "#3366cc" "Hex" 0.5)
       :rgb (sym/->ColorSymbol {"r" (sym/number 10) "g" (sym/number 20) "b" (sym/number 30)} "Rgb" 0.25)
       :hsl (interp/color->type cfg (sym/color "#40bf40" "Hex" 0.75) "hsl")
       :oklch (interp/color->type cfg red "oklch")
       :lab (interp/color->type cfg (sym/color "#123456" "Hex" nil) "lab")})))

(defn- ->symbol
  "Symbol of an argument description, a fresh one for containers."
  [[kind a b]]
  (case kind
    :n (sym/number a)
    :u (sym/number-with-unit a b)
    :color (get @colors a)
    :list (sym/list (mapv ->symbol a) b)
    :str (sym/string a)
    :bool (sym/boolean a)
    :null (sym/null)
    :dict (sym/dictionary {"r" (sym/number 1)})))

(def ^:private gen-number
  (gen/elements [0 1 0.5 0.25 0.999 1.5 2 50 99.5 100 120 127.5 128.4999 254.5 255 255.5
                 300 360 480 -0.5 -5 -120 1e9 ##Inf ##-Inf ##NaN]))

(def ^:private gen-numeric
  (gen/frequency [[6 (gen/fmap (fn [v] [:n v]) gen-number)]
                  [3 (gen/fmap (fn [v] [:u v "%"]) gen-number)]
                  [1 (gen/fmap (fn [v] [:u v "deg"]) gen-number)]]))

(def ^:private gen-color
  (gen/fmap (fn [c] [:color c]) (gen/elements [:hex :hex-alpha :rgb :hsl :oklch :lab])))

(def ^:private gen-other
  (gen/elements [[:n nil] [:u nil "%"] [:u 10 "px"] [:u 50 "rem"] [:str "a"] [:str "red"]
                 [:bool true] [:null] [:dict] [:list [] true] [:list [[:n 1] [:n 2] [:n 3]] false]]))

(def ^:private gen-arg
  (gen/frequency [[6 gen-numeric] [1 gen-color] [2 gen-other]]))

(def ^:private gen-args
  "Argument lists: well formed ones, mostly, and anything."
  (let [channels (gen/tuple gen-numeric gen-numeric gen-numeric)
        alpha (gen/vector gen-numeric 0 1)
        extra (gen/vector gen-arg 0 2)
        well-formed (gen/one-of [(gen/fmap #(vec (apply concat %)) (gen/tuple channels alpha))
                                 (gen/fmap #(vec (apply concat %)) (gen/tuple channels alpha extra))
                                 (gen/fmap #(vec (apply concat %)) (gen/tuple (gen/fmap vector gen-color) alpha extra))])
        anything (gen/vector gen-arg 0 6)
        args (gen/frequency [[6 well-formed] [3 anything]])
        ;; `rgb(255 0 0)` passes one implicit list
        wrapped (gen/fmap (fn [[items implicit?]] [[:list items implicit?]])
                          (gen/tuple args gen/boolean))
        nested (gen/fmap (fn [items] [[:list [[:list items true]] true]]) args)]
    (gen/frequency [[6 args] [3 wrapped] [1 nested]])))

(defn- call
  [fn-name args]
  (fn [cfg] ((interp/resolve-function cfg fn-name) (mapv ->symbol args))))

(t/deftest native-matches-script-property-test
  (let [result (tc/quick-check 3000
                               (prop/for-all [fn-name (gen/elements color-functions)
                                              args gen-args]
                                             (same-as-script? (call fn-name args))))]
    (t/is (true? (:pass? result))
          (pr-str (select-keys result [:seed :shrunk])))))

(def ^:private expressions
  "Token values calling the functions, with `{color.primary}` as
  `#ff0000`, `{color.alpha}` as `#3366cc80` and `{hue}` as `120`."
  ["rgb(255, 0, 0)" "rgb(255 0 0)" "RGB(12, 34, 56)" "rgba(255, 0, 0, 0.5)" "rgba(255 0 0 50%)"
   "rgb(100%, 50%, 0%)" "rgb(33.3%, 66.6%, 99.9%)" "rgb(0.5%, 150%, -10%)"
   "rgb(300, -5, 0)" "rgb(255.5, 254.5, 127.5)" "rgb(0.5, 1.5, -0.5)" "rgb(-0.4, 0.4999, 0.5)"
   "rgba(255, 0, 0, 2)" "rgba(255, 0, 0, -1)" "rgba(255, 0, 0, 150%)" "rgba(255, 0, 0, -5%)"
   "rgba(255, 0, 0, 1)" "rgba(255, 0, 0, 100%)" "rgba(255, 0, 0, 0)" "rgba(255, 0, 0)"
   "rgb(255, 0, 0, 0.5, 7)" "rgb(1, 2, 3, 4, 5, 6)"
   "rgba({color.primary}, 0.5)" "rgba({color.primary} 0.5)" "rgba({color.primary}, 50%)"
   "rgba({color.alpha})" "rgba({color.alpha}, 1)" "rgba({color.alpha}, 0.2, 9)" "rgb({color.primary})"
   "rgba(rgba({color.primary}, 0.5), 1)" "rgba(hsl(120, 100%, 50%), 0.5)"
   "rgb(1, 2)" "rgb(1)" "rgb()" "rgb(a, b, c)" "rgb(10px, 0, 0)" "rgb(true, 0, 0)" "rgb(0, 0, null)"
   "rgb(1, 2, 3, a)" "rgb(1, 2, 3, 10px)" "rgb(1 2, 3)" "rgb(1, 2 3)" "rgba({color.primary}, a)"
   "rgb(255 0 0 / 50%)"
   "hsl(0, 100%, 50%)" "hsl(0 100% 50%)" "hsl(120deg, 100%, 50%)" "hsl(-30deg 50% 50%)"
   "hsl({hue}, 50%, 50%)" "hsl(-120, 100%, 50%)" "hsl(480, 100%, 50%)" "hsl(720, 1, 0.5)"
   "hsl(0, 1, 0.5)" "hsl(240, 1, 0.5)" "hsl(0, 100, 50)" "hsl(120, 50, 50)" "hsl(0, 100%, 0.5)"
   "hsl(0, 0.5, 50)" "hsl(0, 50%, 2)" "hsl(0, 150%, -50%)" "hsl(0, -1, 1)" "hsl(0.5, 0.5, 0.5)"
   "hsla(0, 100%, 50%, 0.5)" "hsla(0, 100%, 50%, 50%)" "hsla(0 100% 50% 50%)" "hsla(0, 1, 0.5, 2)"
   "hsla(0, 1, 0.5, 1)" "hsla({color.primary}, 0.5)" "hsla({color.alpha})" "hsl({color.alpha}, 100%)"
   "hsl(0, 100%)" "hsl(a, b, c)" "hsl(10px, 1, 1)" "hsl(0, 10px, 1)" "hsl(0, 1, 1, 10px)"
   "hsl(1rad, 1, 1)" "hsla()"])

(t/deftest native-matches-script-expressions-test
  (let [refs {"color.primary" (sym/color "#ff0000" "Hex" nil)
              "color.alpha" (sym/color "#3366cc" "Hex" 0.5)
              "hue" (sym/number 120)}]
    (doseq [text expressions]
      (t/is (same-as-script? #(interp/evaluate text {:references refs :config % :mode :inline}))
            text))))

(t/deftest native-matches-script-build-test
  (let [tokens (into {"color.primary" {:value "#ff0000"}
                      "color.half" {:value "rgba({color.primary}, 0.5)"}
                      "color.again" {:value "rgba({color.half}, 1)"}
                      "color.hsl" {:value "hsla({color.primary} 25%)"}
                      "color.broken" {:value "rgba({missing}, 0.5)"}
                      "alpha" {:value "0.25"}
                      "color.alpha-ref" {:value "rgba(10, 20, 30, {alpha})"}
                      "color.mixed" {:value "lighten(rgb(10 20 30), 0.2)"}}
                     (map-indexed (fn [i text] [(str "t" i) {:value text}]))
                     expressions)
        run (fn [cfg] (select-keys (proc/build tokens {:config cfg}) [:tokens :resolved :issues]))]
    (t/is (same-as-script? run))
    (t/is (= "#ff000080" (sym/to-string (tsc/color->hex (get-in (run (h/full-config)) [:tokens "color.half"])))))))

(t/deftest native-handles-well-formed-arguments-test
  (let [cfg (h/full-config)
        to-type #(interp/color->type cfg %1 %2)
        native? (fn [f & args] (some? (f cfg (mapv ->symbol args) to-type)))]
    (t/testing "numbers, percentages, deg and colors run natively"
      (t/is (native? css-colors/rgb [:n 255] [:n 0] [:n 0]))
      (t/is (native? css-colors/rgb [:u 50 "%"] [:n 300] [:n -5] [:u 50 "%"]))
      (t/is (native? css-colors/rgb [:list [[:n 1] [:n 2] [:n 3] [:n 0.5]] true]))
      (t/is (native? css-colors/rgb [:color :oklch] [:n 0.5]))
      (t/is (native? css-colors/hsl [:u 120 "deg"] [:u 50 "%"] [:n 0.5] [:u 50 "%"]))
      (t/is (native? css-colors/hsl [:color :hex] [:n 0.5])))
    (t/testing "anything else runs the script"
      (t/is (not (native? css-colors/rgb [:n 1] [:n 2])))
      (t/is (not (native? css-colors/rgb)))
      (t/is (not (native? css-colors/rgb [:list [] true])))
      (t/is (not (native? css-colors/rgb [:u 10 "px"] [:n 0] [:n 0])))
      (t/is (not (native? css-colors/rgb [:n ##NaN] [:n 0] [:n 0])))
      (t/is (not (native? css-colors/rgb [:n 1] [:n 2] [:n 3] [:str "a"])))
      (t/is (not (native? css-colors/hsl [:u 1 "rem"] [:n 1] [:n 1]))))))

(t/deftest native-errors-test
  (t/testing "errors are the script's, wrapped as execution errors"
    (doseq [[text inner] [["rgb(1, 2)" :symbols/index-out-of-range]
                          ["rgb()" :symbols/index-out-of-range]
                          ["rgb(a, b, c)" :interpreter/type-mismatch]
                          ["rgba(255, 0, 0, true)" :interpreter/type-mismatch]
                          ["hsl(0, 100%)" :symbols/index-out-of-range]]]
      (let [error (try (h/evaluate text nil {:config (h/full-config) :mode :inline})
                       nil
                       (catch #?(:clj Throwable :cljs :default) e (err/->error-map e)))]
        (t/is (= :functions/execution-error (:code error)) text)
        (t/is (= (subs text 0 (.indexOf ^String text "(")) (get-in error [:data :function-name])) text)
        (t/is (= inner (get-in error [:data :cause :code])) text)))))

(t/deftest native-registration-test
  (let [functions-of (fn [cfg] (:functions cfg))
        native-of (fn [cfg k] (some-> (functions/dynamic-function (functions-of cfg) k) interp/native-function))
        cfg (h/full-config)]
    (t/testing "the bundled scripts run natively, and keep their schemas"
      (doseq [k color-functions]
        (t/is (some? (native-of cfg k)) k)
        (t/is (nil? (native-of (h/script-config) k)) k)
        (t/is (interp/has-function? cfg k) k)
        (t/is (string? (:description (functions/spec (functions-of cfg) k))) k)
        (t/is (some #{k} (functions/function-names (functions-of cfg))) k)))
    (t/testing "a schema redefining a keyword runs its own script"
      (let [cfg (config/register-schemas cfg [{:uri "/api/v1/function/my-rgb/0/"
                                               :schema {:name "My RGB"
                                                        :type "function"
                                                        :keyword "rgb"
                                                        :script {:script "return {input}.length();"}}}
                                              {:uri "/api/v1/function/double/0/"
                                               :schema {:name "Double"
                                                        :type "function"
                                                        :keyword "double"
                                                        :script {:script "return {input}.get(0) * 2;"}}}])]
        (t/is (nil? (native-of cfg "rgb")))
        (t/is (some? (native-of cfg "rgba")))
        (t/is (= "3" (h/ev "rgb(1, 2, 3)" nil {:config cfg :mode :inline})))
        (t/is (= "8" (h/ev "double(4)" nil {:config cfg :mode :inline})))))
    (t/testing "the bundled schemas registered on another config run natively too"
      (let [cfg (config/register-schemas (config/make-config) (schemas/schemas))]
        (t/is (some? (native-of cfg "hsla")))))))
