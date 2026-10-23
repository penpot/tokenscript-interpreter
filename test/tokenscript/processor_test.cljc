;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns tokenscript.processor-test
  (:require
   [clojure.test :as t]
   [clojure.test.check :as tc]
   [clojure.test.check.generators :as gen]
   [clojure.test.check.properties :as prop]
   [tokenscript.config :as config]
   [tokenscript.core :as tsc]
   [tokenscript.errors :as err]
   [tokenscript.processor :as proc]
   [tokenscript.symbols :as sym]
   [tokenscript.test-helpers :as h]
   [tokenscript.token-data :as td]))

(defn- process
  ([tokens] (process tokens (h/core-config)))
  ([tokens cfg] (proc/build tokens {:config cfg})))

(defn- value
  "String form of a resolved token."
  [res name]
  (let [v (get-in res [:tokens name])]
    (if (sym/symbol? v) (sym/to-string v (get-in res [:state :config])) v)))

(defn- codes
  [res name]
  (mapv :code (get-in res [:issues name])))

(t/deftest simple-resolution-test
  (let [res (process {"colors.primary" "#FF0000"
                      "colors.secondary" "{colors.primary}"
                      "spacing.base" "8"
                      "spacing.small" "{spacing.base} / 2"
                      "spacing.large" "{spacing.base} * 2px"
                      "a" "10" "b" "{a} + 5" "c" "{b} * 2"})]
    (t/is (= "#FF0000" (value res "colors.secondary")))
    (t/is (= "4" (value res "spacing.small")))
    (t/is (= "16px" (value res "spacing.large")))
    (t/is (= "30" (value res "c")))
    (t/is (nil? (:issues res)))
    (t/is (= #{"b"} (get-in res [:graph "c"])))
    (t/is (= #{} (get-in res [:graph "a"])))))

(t/deftest primitive-values-test
  (let [res (process {"n" 8 "b" true "nil" nil "kw" "uppercase" "ref" "{n} + 1"})]
    (t/is (= "8" (value res "n")))
    (t/is (= "Number" (sym/type-name (get-in res [:tokens "n"]))))
    (t/is (= "true" (value res "b")))
    (t/is (sym/null? (get-in res [:tokens "nil"])))
    (t/is (= "uppercase" (value res "kw")) "uninterpreted keywords stay strings")
    (t/is (= "9" (value res "ref")))))

(t/deftest errors-test
  (let [res (process {"a" "10"
                      "bad" "{a} + invalid syntax !@#"
                      "miss" "{nope}"
                      "miss2" "{miss} + 1"})]
    (t/is (= "10" (value res "a")))
    (t/is (= "{a} + invalid syntax !@#" (value res "bad")) "failed tokens keep their original value")
    (t/is (= [:lexer/invalid-character] (codes res "bad")))
    (t/is (= [:processor/dependency-error] (codes res "miss")))
    (t/is (= ["miss" "nope"] (get-in res [:issues "miss" 0 :data :chain])))
    (t/is (= :processor/token-not-found (get-in res [:issues "miss" 0 :data :cause :code])))
    (t/is (= ["miss2" "miss" "nope"] (get-in res [:issues "miss2" 0 :data :chain])) "chains accumulate")
    (t/is (= :processor/token-not-found (get-in res [:issues "miss2" 0 :data :cause :code])) "the root cause is kept")
    (t/is (= #{:type :kind :code :data} (set (keys (get-in res [:issues "miss2" 0])))) "issues carry no messages")
    (t/is (not (contains? (:resolved res) "nope")) "missing tokens are not invented")
    (t/is (= #{"miss" "miss2"} (proc/broken-references res)))
    (t/is (err/error-map? (proc/token-error res "bad")))
    (t/is (proc/issue-with-code? res "miss" :processor/dependency-error))))

(t/deftest circular-dependencies-test
  (let [res (process {"x" "{y}" "y" "{x}" "z" "{x} + 1" "self" "{self}"})]
    (t/is (= [:processor/circular-dependency] (codes res "x")))
    (t/is (= [:processor/circular-dependency] (codes res "y")))
    (t/is (= [:processor/circular-dependency] (codes res "self")))
    (t/is (= [:processor/dependency-error] (codes res "z")) "dependents of a cycle get a dependency error")
    (t/is (= :processor/circular-dependency (get-in res [:issues "z" 0 :data :cause :code]))))
  (t/testing "cycles through groups and dictionary keys"
    (let [res (process {"a" "{g}" "g.x" "{a}" "g.y" "{other}" "other" "1"
                        "d" "{grp}" "grp.k" "{d.k}"})]
      (t/is (= [:processor/circular-dependency] (codes res "a")))
      (t/is (= "1" (value res "g.y")) "a group member off the cycle resolves")
      (t/is (nil? (get-in res [:issues "g.y"])))
      (t/is (= [:processor/circular-dependency] (codes res "d")) "a key of a token being resolved closes a cycle")
      (t/is (= :processor/circular-dependency
               (get-in res [:issues "grp.k" 0 :data :cause :code] (get-in res [:issues "grp.k" 0 :code])))))))

(t/deftest group-references-test
  (let [res (process {"color.primary" "#FF0000"
                      "color.secondary" "{color.primary}"
                      "color.nested.deep" "#000"
                      "group" "{color}"
                      "keys" "{color}.keys()"
                      "pick" "{color}.get(\"secondary\")"})]
    (t/is (= "Dictionary" (sym/type-name (get-in res [:tokens "group"]))))
    (t/is (= "primary, secondary" (value res "keys")) "only direct children are included")
    (t/is (= "#FF0000" (value res "pick")))))

(t/deftest dictionary-children-test
  (let [res (process {"color.primary" "#FF0000"
                      "dict" "{color}"
                      "virt" "{dict.primary}"
                      "missing" "{dict.nope}"})]
    (t/is (= "#FF0000" (value res "virt")) "keys of dictionary tokens are addressable")
    (t/is (= [:processor/dependency-error] (codes res "missing")))))

(t/deftest structured-tokens-test
  (let [res (process {"base" "4"
                      "shadow" {:value {:offset-x "{base} * 2" :offset-y 4 :blur "{missing}" :color "#000"}
                                :type "shadow"}
                      "shadows" {:value [{:blur "{base}px" :color "#fff"}] :type "shadow"}
                      "plain" {:value {:a 1 :b "static"} :type "custom"}
                      "ref" "{shadows[0].blur}"
                      "ref2" "{shadow.offset-x}"})]
    (t/is (= "Token.Shadow" (sym/full-type-name (get-in res [:tokens "shadows"]))))
    (t/is (= "{blur: 4px, color: #fff}" (value res "shadows")))
    (t/is (= [4.0 "#fff"] (let [d (sym/->data (get-in res [:tokens "shadows"]))]
                            [(double (get-in d [0 "blur" :value])) (get-in d [0 "color" :value])])))
    (t/is (= "4px" (value res "ref")) "fields of structured tokens are referenceable")
    (t/is (= "8" (value res "ref2")))
    (t/is (= "{a: 1, b: static}" (value res "plain")))
    (t/testing "a failing field fails the parent with the field path"
      (t/is (map? (value res "shadow")))
      (let [[field-issue parent-issue] (get-in res [:issues "shadow"])]
        (t/is (= ["blur"] (:path field-issue)))
        (t/is (= :processor/dependency-error (:code field-issue)))
        (t/is (err/error-map? field-issue))
        (t/is (= :processor/token-not-found (get-in field-issue [:data :cause :code])))
        (t/is (= :processor/dependency-error (:code parent-issue)))))))

(t/deftest object-parsers-test
  (let [res (proc/build {"spacing" {:value {:small {:value 8 :unit "px"}} :type "spacing"}}
                        {:config (h/core-config)
                         :object-parsers [td/number-with-unit-parser]})]
    (t/is (= "{small: 8px}" (value res "spacing")))))

(t/deftest token-name-validation-test
  (let [res (process {"bad name" "1" "ok.name" "2"})]
    (t/is (= [:processor/invalid-token-name] (codes res "bad name")))
    (t/is (= "1" (value res "bad name")) "the token still resolves")
    (t/is (nil? (get-in res [:issues "ok.name"])))))

(t/deftest inline-constants-test
  (let [res (process {"a" "red" "b" "{a}" "c" "lighten({b}, 0.1)"} (h/full-config))]
    (t/is (= "#FF0000" (value res "a")) "css color names come from the bundled constants")
    (t/is (= "Color.Oklch" (sym/full-type-name (get-in res [:tokens "c"]))))
    (t/is (nil? (:issues res)))))

(t/deftest type-validation-test
  (let [cfg (config/register-schema (h/core-config)
                                    {:uri "https://example.com/api/v1/schema/opacity/0/"
                                     :schema {:name "opacity"
                                              :type "token"
                                              :validation "if (is_number({input}) == false) [ return \"NOT_A_NUMBER\"; ]; if ({input} > 1) [ return \"OUT_OF_RANGE\"; ]; return true;"}})
        res (process {"ok" {:value "0.5" :type "opacity"}
                      "big" {:value "2" :type "opacity"}
                      "str" {:value "\"a\"" :type "opacity"}} cfg)]
    (t/is (nil? (get-in res [:issues "ok"])))
    (t/is (= ["OUT_OF_RANGE"] (codes res "big")))
    (t/is (= :warning (get-in res [:issues "big" 0 :severity])))
    (t/is (= ["NOT_A_NUMBER"] (codes res "str")))
    (t/is (= "2" (value res "big")) "validation issues do not fail the token")))

(t/deftest update-token-test
  (let [res (process {"a" "10" "b" "{a} + 5" "c" "{b} * 2" "d" "1"})
        st (:state res)
        upd (proc/update-token st "a" {:value "20"})]
    (t/is (= "20" (value upd "a")))
    (t/is (= "25" (value upd "b")))
    (t/is (= "50" (value upd "c")))
    (t/is (= #{"a" "b" "c"} (proc/affected-tokens upd)))
    (t/is (= #{"a" "b" "c"} (proc/modified-dependents (:tokens res) upd)))
    (t/is (= "1" (value upd "d")) "unaffected tokens keep their value")
    (t/testing "updating an unknown token creates it"
      (t/is (= "2" (value (proc/update-token st "e" {:value "{d} + 1"}) "e"))))
    (t/testing "a value can be broken and fixed again"
      (let [broken (proc/update-token st "a" {:value "{zzz}"})
            fixed (proc/update-token (:state broken) "a" {:value "1"})]
        (t/is (= #{"a" "b" "c"} (proc/broken-references broken)))
        (t/is (nil? (:issues fixed)))
        (t/is (= "12" (value fixed "c")))))))

(t/deftest rename-token-test
  (let [res (process {"a" "10" "b" "{a} + 5"})
        rn (proc/rename-token (:state res) "a" "alpha" {:update-references :visible})]
    (t/is (= "{alpha} + 5" (get-in rn [:state :tokens "b"])) "the data is rewritten in the shape it was given")
    (t/is (= "{alpha} + 5" (get-in rn [:state :data "b" :value])))
    (t/is (= "15" (value rn "b")))
    (t/is (= #{"alpha" "b"} (proc/affected-tokens rn)))
    (t/is (not (contains? (:tokens rn) "a"))))
  (t/testing "without reference updates the dependents break"
    (let [res (process {"a" "10" "b" "{a} + 5"})
          rn (proc/rename-token (:state res) "a" "alpha")]
      (t/is (= #{"b"} (proc/broken-references rn)))))
  (let [st (:state (process {"a" "1" "b" "2"}))]
    (t/is (= :processor/token-already-exists (h/error-code #(proc/rename-token st "a" "b"))))
    (t/is (= :processor/token-not-found (h/error-code #(proc/rename-token st "x" "y")))))
  (t/testing "a token whose data is nil"
    (let [rn (proc/rename-token (:state (process {"a" nil "b" "1"})) "a" "alpha")]
      (t/is (contains? (get-in rn [:state :tokens]) "alpha"))
      (t/is (sym/null? (get-in rn [:tokens "alpha"]))))))


(defn- build-sets
  [sets]
  (proc/build sets {:config (h/core-config)}))

(t/deftest rename-token-data-test
  (t/testing "DTCG data keeps its keys"
    (let [res (process {"a" {"$value" "10"} "b" {"$value" "{a} + 5" "$type" "number"}})
          rn (proc/rename-token (:state res) "a" "alpha" {:update-references :visible})]
      (t/is (= {"$value" "{alpha} + 5" "$type" "number"} (proc/input rn "b")))
      (t/is (= {"$value" "10"} (proc/input rn "alpha")))
      (t/is (= "15" (value rn "b")))))
  (t/testing "normalized data keeps its other fields"
    (let [res (process {"a" {:value "10"} "b" {:value "{a} + 5" :type "number" :metadata 7}})
          rn (proc/rename-token (:state res) "a" "alpha" {:update-references :visible})]
      (t/is (= {:value "{alpha} + 5" :type "number" :metadata 7} (proc/input rn "b")))))
  (t/testing "a caller's records"
    (let [reader (fn [{:keys [val]}] {:value val})
          records {"a" {:name "a" :val "10" :id 1}
                   "b" {:name "b" :val "{a} + 5" :id 2}
                   "c" {:name "c" :val "{b} * 2" :id 3}}
          st (:state (proc/build records {:config (h/core-config) :token-reader reader}))
          e (try (proc/rename-token st "a" "alpha" {:update-references :visible})
                 nil
                 (catch #?(:clj Exception :cljs :default) e (ex-data e)))]
      (t/testing "can not be rewritten"
        (t/is (= :processor/token-not-updatable (:code e)))
        (t/is (= {:token-name "b"} (:data e))))
      (t/testing "get the rewritten values to write themselves"
        (t/is (= {[nil "b"] "{alpha} + 5"} (proc/reference-updates st "a" "alpha"))))
      (t/testing "are moved as they are without reference updates"
        (let [rn (proc/rename-token st "a" "alpha")]
          (t/is (identical? (get records "a") (proc/input rn "alpha")))
          (t/is (= #{"b" "c"} (proc/broken-references rn))))))))

;; A rename in the set a token is taken from lets a definition it shadowed
;; through, and rewrites the references to the visible token only.
(t/deftest rename-token-sets-test
  (t/testing "a shadowed definition becomes visible"
    (let [res (build-sets [[:a {"token-foo" "#ff0000"}] [:b {"token-foo" "#0000ff"}]])
          rn (proc/rename-token (:state res) "token-foo" "token-bar")]
      (t/is (= "#ff0000" (value rn "token-foo")))
      (t/is (= "#0000ff" (value rn "token-bar")))
      (t/is (= :a (proc/source rn "token-foo")))
      (t/is (= :b (proc/source rn "token-bar")))))
  (let [core {"a" "1" "b" "{a} * 10"}
        theme {"a" "2" "b" "{a} + 1" "c" "{a} + 100"}
        res (build-sets [[:core core] [:theme theme]])
        data (fn [res set-key name] (get-in (into {} (get-in res [:state :sets])) [set-key name]))]
    (t/testing ":visible rewrites the visible definitions"
      (let [rn (proc/rename-token (:state res) "a" "alpha" {:update-references :visible})]
        (t/is (= "{alpha} + 1" (data rn :theme "b")))
        (t/is (= "{alpha} + 100" (data rn :theme "c")))
        (t/is (= "{a} * 10" (data rn :core "b")) "a shadowed definition is kept")
        (t/is (= "1" (value rn "a")))
        (t/is (= "3" (value rn "b")))
        (t/is (= "102" (value rn "c")))))
    (t/testing ":all rewrites every definition"
      (let [rn (proc/rename-token (:state res) "a" "alpha" {:update-references :all})]
        (t/is (= "{alpha} * 10" (data rn :core "b")))
        (t/is (= "{alpha} + 1" (data rn :theme "b")))
        (t/is (= {[:core "b"] "{alpha} * 10" [:theme "b"] "{alpha} + 1" [:theme "c"] "{alpha} + 100"}
                 (proc/reference-updates (:state res) "a" "alpha" {:update-references :all})))))
    (t/testing "renaming a shadowed definition keeps the references to the visible one"
      (let [rn (proc/rename-token (:state res) "a" "alpha" {:set :core :update-references :all})]
        (t/is (= "{a} * 10" (data rn :core "b")))
        (t/is (= "1" (value rn "alpha")))
        (t/is (= "3" (value rn "b")))
        (t/is (= "102" (value rn "c")))
        (t/is (= {} (proc/reference-updates (:state res) "a" "alpha" {:set :core :update-references :all})))))))

(t/deftest rename-token-references-test
  (t/testing "in structured values"
    (let [res (process {"a" "2" "typo" {:fontSize "{a} * 8" :lineHeight "{a}" :weight "400"}})
          rn (proc/rename-token (:state res) "a" "alpha" {:update-references :visible})]
      (t/is (= {:fontSize "{alpha} * 8" :lineHeight "{alpha}" :weight "400"} (proc/input rn "typo")))
      (t/is (nil? (:issues rn)))))
  (t/testing "in vectors of structured values"
    (let [res (process {"a" "2" "shadow" [{:blur "{a}"} {:blur "3"}]})
          rn (proc/rename-token (:state res) "a" "alpha" {:update-references :visible})]
      (t/is (= [{:blur "{alpha}"} {:blur "3"}] (proc/input rn "shadow")))))
  (t/testing "to a field of the renamed token"
    (let [res (process {"typo" {:fontSize "16"} "size" "{typo.fontSize} * 2"})
          rn (proc/rename-token (:state res) "typo" "heading" {:update-references :visible})]
      (t/is (= "{heading.fontSize} * 2" (proc/input rn "size")))
      (t/is (= "32" (value rn "size")))))
  (t/testing "but not to a token below its name"
    (let [res (process {"a" "1" "a.x" "2" "b" "{a} + {a.x}"})
          rn (proc/rename-token (:state res) "a" "alpha" {:update-references :visible})]
      (t/is (= "{alpha} + {a.x}" (proc/input rn "b")))
      (t/is (= "3" (value rn "b")))))
  (t/testing "in the renamed token itself"
    (let [res (process {"a" "{a}"})
          rn (proc/rename-token (:state res) "a" "alpha" {:update-references :visible})]
      (t/is (= "{alpha}" (proc/input rn "alpha"))))))

(t/deftest assoc-value-test
  (t/is (= "{b}" (td/assoc-value "{a}" "{b}")))
  (t/is (= {"$value" "{b}" "$type" "number"} (td/assoc-value {"$value" "{a}" "$type" "number"} "{b}")))
  (t/is (= {:$value "{b}"} (td/assoc-value {:$value "{a}"} "{b}")))
  (t/is (= {:value "{b}" "$value" "{a}"} (td/assoc-value {:value "{a}" "$value" "{a}"} "{b}"))
        "the value is written where it is read")
  (t/is (= {:p "{b}"} (td/assoc-value {:p "{a}"} {:p "{b}"})) "a structured value is replaced"))

(t/deftest create-and-delete-token-test
  (let [res (process {"a" "10" "b" "{a} + 5"})
        cr (proc/create-token (:state res) "c" {:value "{b} * 2"})]
    (t/is (= "30" (value cr "c")))
    (t/is (= :processor/token-already-exists
             (h/error-code #(proc/create-token (:state cr) "c" {:value "1"}))))
    (let [del (proc/delete-token (:state cr) "a")]
      (t/is (not (contains? (:tokens del) "a")))
      (t/is (= #{"b" "c"} (proc/broken-references del)))
      (t/is (= #{"b" "c"} (proc/affected-tokens del)))
      (t/is (= :processor/token-not-found
               (h/error-code #(proc/delete-token (:state del) "a")))))))

(defn- snapshot
  "Resolved strings and issue codes of a result, to compare results."
  [res]
  {:tokens (proc/tokens->strings res)
   :issues (update-vals (or (:issues res) {}) #(frequencies (map :code %)))})

(defn- same-as-build?
  "Whether an incremental result equals a fresh build of its sets."
  [res]
  (= (snapshot (proc/build (get-in res [:state :sets]) {:config (get-in res [:state :config])}))
     (snapshot res)))


(t/deftest sets-test
  (let [res (build-sets [[:core {"color.bg" {:value "#fff" :metadata {:id 1}}
                                 "space" "8"}]
                         [:dark {"color.bg" {:value "#000" :metadata {:id 2}}
                                 "double" "{space} * 2"}]])]
    (t/is (= "#000" (value res "color.bg")) "later sets win")
    (t/is (= "16" (value res "double")) "references reach across sets")
    (t/is (= :dark (proc/source res "color.bg")))
    (t/is (= :core (proc/source res "space")))
    (t/is (= {:id 2} (proc/metadata res "color.bg")))
    (t/is (= :processor/token-set-not-found
             (h/error-code #(proc/update-token (:state res) "x" {:value "1"} {:set :nope}))))))

(t/deftest shadowed-update-test
  (let [res (build-sets [[:core {"a" "1" "b" "{a} + 1"}]
                         [:dark {"a" "10"}]])
        shadowed (proc/update-token (:state res) "a" {:value "2"} {:set :core})]
    (t/is (= "11" (value shadowed "b")))
    (t/is (= {} (:dependents shadowed)) "an overridden definition affects nothing")
    (t/is (identical? (get-in res [:tokens "b"]) (get-in shadowed [:tokens "b"])))
    (t/testing "deleting the override uses the earlier definition"
      (let [del (proc/delete-token (:state shadowed) "a" {:set :dark})]
        (t/is (= "2" (value del "a")))
        (t/is (= "3" (value del "b")))
        (t/is (same-as-build? del))))
    (t/testing "by default operations apply to the set a token is taken from"
      (let [upd (proc/update-token (:state res) "a" {:value "20"})]
        (t/is (= "20" (get-in upd [:state :sets 1 1 "a" :value])))
        (t/is (= "1" (get-in upd [:state :sets 0 1 "a"])) "the other set keeps its data as given")))))

;; A caller's own token records are kept as given and read by the
;; `:token-reader` when a token is parsed; unchanged records are skipped by
;; identity.
(t/deftest token-reader-test
  (let [reader (fn [{:keys [val kind id]}]
                 (if (nil? val)
                   (err/error-map :reader/empty-value {:data {:id id}})
                   (cond-> {:value val :metadata id}
                     kind (assoc :type (name kind)))))
        build (fn [sets] (proc/build sets {:config (h/core-config) :token-reader reader}))
        a {:val "8" :kind :number :id 1}
        b {:val "{a} * 2" :id 2}
        c {:val "1" :id 3}
        res (build [[:core {"a" a "b" b "c" c}]])]
    (t/is (= "16" (value res "b")))
    (t/is (= 1 (proc/metadata res "a")))
    (t/is (identical? a (get-in res [:state :tokens "a"])) "the data is stored as given")
    (t/testing "an operation re-reads only the tokens whose data changed"
      (let [reads (atom [])
            counting (fn [data] (swap! reads conj (:id data)) (reader data))
            res (proc/build [[:core {"a" a "b" b "c" c}]] {:config (h/core-config) :token-reader counting})
            _ (reset! reads [])
            upd (proc/set-sets (:state res) [[:core {"a" (assoc a :val "10") "b" b "c" c}]])]
        (t/is (= "20" (value upd "b")))
        (t/is (= [1] @reads))
        (t/is (identical? (get-in res [:tokens "c"]) (get-in upd [:tokens "c"])))))
    (t/testing "operations take the caller's shape too"
      (let [upd (proc/update-token (:state res) "a" {:val "3" :kind :number :id 1})]
        (t/is (= "6" (value upd "b")))))
    (t/testing "an error map from the reader fails the token and its dependents"
      (let [res (build [[:core {"a" {:val nil :id 1} "b" b}]])]
        (t/is (= [:reader/empty-value] (codes res "a")))
        (t/is (nil? (get-in res [:tokens "a"])))
        (t/is (= [:processor/dependency-error] (codes res "b")))
        (t/is (= :reader/empty-value (get-in res [:issues "b" 0 :data :cause :code])))))
    (t/testing "type validation reads the type from the reader"
      (let [res (proc/build [[:core {"a" {:val "8px" :kind :number :id 1}
                                     "early" {:val 8 :kind :color :id 2}}]]
                            {:config (tsc/default-config) :token-reader reader})]
        (t/is (seq (codes res "a")))
        (t/is (seq (codes res "early")) "also for a value resolved while parsing")))))

;; The `:token-writer` turns each token into the caller's output entry; an
;; operation writes only the names it changed.
(t/deftest token-writer-test
  (let [cfg (h/core-config)
        writes (atom [])
        writer (fn [token value issues]
                 (swap! writes conj token)
                 {:token token
                  :value (if (sym/symbol? value) (sym/to-string value cfg) value)
                  :codes (mapv :code issues)})
        core {"base" "8" "double" "{base} * 2" "other" "1" "bad" "{missing}"}
        res (proc/build [[:core core]] {:config cfg :token-writer writer})]
    (t/is (= {:token "{base} * 2" :value "16" :codes []} (get-in res [:output "double"])))
    (t/is (= [:processor/dependency-error] (get-in res [:output "bad" :codes])))
    (t/is (nil? (:output (proc/build [[:core core]] {:config cfg}))) "no writer, no output")
    (t/testing "an operation writes only the changed names"
      (reset! writes [])
      (let [upd (proc/set-sets (:state res) [[:core (assoc core "base" "10")]])]
        (t/is (= #{"10" "{base} * 2"} (set @writes)))
        (t/is (= "20" (get-in upd [:output "double" :value])))
        (t/is (identical? (get-in res [:output "other"]) (get-in upd [:output "other"])))))
    (t/testing "a removed token leaves the output"
      (t/is (not (contains? (:output (proc/delete-token (:state res) "other")) "other"))))))

(t/deftest set-sets-changed-test
  (let [core {"base" "8" "double" "{base} * 2" "other" "1"}
        theme {"accent" "#f00"}
        res (build-sets [[:core core] [:theme theme]])]
    (t/testing "the same sets change nothing"
      (t/is (= #{} (:changed (proc/set-sets (:state res) [[:core core] [:theme theme]])))))
    (t/testing "an edit in one set changes the edited name and its dependents"
      (let [upd (proc/set-sets (:state res) [[:core (assoc core "base" "10")] [:theme theme]])]
        (t/is (= #{"base" "double"} (:changed upd)))
        (t/is (= "20" (value upd "double")))
        (t/is (identical? (get-in res [:tokens "other"]) (get-in upd [:tokens "other"])))))
    (t/testing "adding and removing sets changes the names they define"
      (let [added (proc/set-sets (:state res) [[:core core] [:mid {"other" "5"}] [:theme theme]])
            removed (proc/set-sets (:state res) [[:core core]])]
        (t/is (= #{"other"} (:changed added)))
        (t/is (= #{"accent"} (:changed removed)))
        (t/is (not (contains? (:tokens removed) "accent")))))
    (t/testing "reordering sets compares every name"
      (let [res (build-sets [[:a {"x" "1"}] [:b {"x" "2"}]])
            swapped (proc/set-sets (:state res) [[:b {"x" "2"}] [:a {"x" "1"}]])]
        (t/is (= "1" (value swapped "x")))
        (t/is (= #{"x"} (:changed swapped)))))
    (t/testing "the winning data is readable as given"
      (t/is (= "#f00" (proc/input res "accent"))))))

(t/deftest set-sets-test
  (let [core {"base" "8" "other" "{base} + 1" "color.bg" "{mode.bg}"}
        res (build-sets [[:core core] [:light {"mode.bg" "#fff"}]])
        dark (proc/set-sets (:state res) [[:core core] [:dark {"mode.bg" "#000"}]])]
    (t/is (= "#000" (value dark "color.bg")))
    (t/is (identical? (get-in res [:tokens "other"]) (get-in dark [:tokens "other"]))
          "tokens that cannot see the change keep their value")
    (t/is (same-as-build? dark))
    (t/testing "removing a set"
      (let [none (proc/set-sets (:state dark) [[:core core]])]
        (t/is (= [:processor/dependency-error] (codes none "color.bg")))
        (t/is (same-as-build? none))))))

(t/deftest incremental-references-test
  (let [res (process {"color.primary" "#FF0000"
                      "group" "{color}"
                      "pick" "{group.primary}"
                      "base" "4"
                      "shadow" {:value {:blur "{base} * 2"} :type "shadow"}
                      "blur" "{shadow.blur}"
                      "waiting" "{later}"
                      "bad name" "{base}"})
        st (:state res)]
    (t/testing "groups and dictionary keys follow their members"
      (let [upd (proc/update-token st "color.primary" {:value "#00FF00"})]
        (t/is (= "#00FF00" (value upd "pick")))
        (t/is (same-as-build? upd)))
      (let [cr (proc/create-token st "color.extra" {:value "#000"})]
        (t/is (= "Dictionary" (sym/type-name (get-in cr [:tokens "group"]))))
        (t/is (= "2" (sym/to-string (sym/length (get-in cr [:tokens "group"])))))
        (t/is (same-as-build? cr))))
    (t/testing "fields of structured tokens follow their references"
      (let [upd (proc/update-token st "base" {:value "5"})]
        (t/is (= "10" (value upd "blur")))
        (t/is (= [:processor/invalid-token-name] (codes upd "bad name")) "re-resolved tokens keep their name issue")
        (t/is (same-as-build? upd))))
    (t/testing "creating a missing token fixes its dependents"
      (let [cr (proc/create-token st "later" {:value "1"})]
        (t/is (= "1" (value cr "waiting")))
        (t/is (nil? (get-in cr [:issues "waiting"])))
        (t/is (same-as-build? cr))))
    (t/testing "the state an operation started from is unchanged"
      (proc/update-token st "base" {:value "100"})
      (t/is (= "8" (value res "blur"))))))

(t/deftest incremental-matches-build-test
  (let [ops [[:update "a" "{c}"] [:update "c" "1"] [:update "c" "{a}"] [:delete "b"]
             [:create "b" "{a} * 3"] [:update "a" "{missing}"] [:create "missing" "2"]
             [:update "s" "{x}"] [:rename "a" "z"] [:update "d" {:p "{z}" :q "{b}"}]]
        start (:state (process {"a" "10" "b" "{a} + 5" "c" "{b} * 2" "d" {:p "{a}" :q "1"}
                                "g.x" "{b}" "g.y" "2" "s" "{g}" "x" "{s}"}))]
    (reduce (fn [st [op name arg]]
              (let [res (case op
                          :update (proc/update-token st name {:value arg})
                          :create (proc/create-token st name {:value arg})
                          :delete (proc/delete-token st name)
                          :rename (proc/rename-token st name arg {:update-references :visible}))]
                (t/is (same-as-build? res) (str "after " op " " name))
                (:state res)))
            start
            ops)))

(def ^:private prop-names
  ["a" "b" "c" "g.x" "g.y" "d" "dict" "bad name"])

(def ^:private prop-values
  (into ["1" "2" 3 nil "" "1 +" "uppercase" "{g}" "{dict.x}" "{d.p}"
         "variable q: Number = 2; q * 3" {:p "{a}" :q "2"} [{:p "{b} * 2"}]]
        (mapcat (fn [n] [(str "{" n "}") (str "{" n "} + 1")]))
        prop-names))

(def ^:private prop-set-keys
  [:core :light :dark])

(def ^:private gen-set-keys
  "Non-empty subsequences of `prop-set-keys`."
  (->> (apply gen/tuple (repeat (count prop-set-keys) gen/boolean))
       (gen/fmap (fn [picks] (into [] (keep (fn [[pick k]] (when pick k))) (map vector picks prop-set-keys))))
       (gen/such-that seq)))

(def ^:private gen-sets
  (let [tokens (gen/map (gen/elements prop-names) (gen/elements prop-values) {:max-elements 6})]
    (gen/bind gen-set-keys
              (fn [ks] (gen/fmap #(mapv vector ks %) (apply gen/tuple (repeat (count ks) tokens)))))))

(def ^:private gen-op
  (let [token-name (gen/elements prop-names)
        set-key (gen/elements [nil :core :dark])]
    (gen/one-of [(gen/tuple (gen/elements [:update :create]) token-name (gen/elements prop-values) set-key)
                 (gen/tuple (gen/elements [:delete]) token-name set-key)
                 (gen/tuple (gen/elements [:rename]) token-name token-name (gen/elements [nil :visible :all]))
                 (gen/tuple (gen/elements [:sets]) gen-sets)
                 ;; an edit of the current sets, keeping the other set maps
                 ;; and token data as the same objects, or no edit
                 (gen/tuple (gen/elements [:edit]) gen/nat token-name (gen/elements (into [::delete ::keep] prop-values)))])))

(defn- apply-op
  "Result of `op` on `st`, nil when the operation is refused."
  [st [op a b c]]
  (try
    (case op
      :update (proc/update-token st a {:value b} (when c {:set c}))
      :create (proc/create-token st a {:value b} (when c {:set c}))
      :delete (proc/delete-token st a (when b {:set b}))
      :rename (proc/rename-token st a b {:update-references c})
      :sets (proc/set-sets st a)
      :edit (let [sets (:sets st)
                  i (mod a (count sets))]
              (proc/set-sets st (case c
                                  ::keep sets
                                  ::delete (update-in sets [i 1] dissoc b)
                                  (update-in sets [i 1] assoc b c)))))
    (catch #?(:clj Exception :cljs :default) e
      (when-not (contains? #{:processor/token-not-found :processor/token-already-exists :processor/token-set-not-found}
                           (:code (ex-data e)))
        (throw e)))))

(defn- normalize-cycle-issues
  "Result where a dependency error caused by a cycle counts as a circular
  dependency, as a consumer shows both. Which tokens of a cycle get which of
  the two depends on where resolution enters the cycle, and that differs
  between a build and an update."
  [res]
  (update res :issues
          (fn [issues]
            (when issues
              (update-vals issues
                           (fn [is]
                             (mapv #(if (and (= :processor/dependency-error (:code %))
                                             (= :processor/circular-dependency (get-in % [:data :cause :code])))
                                      (assoc % :code :processor/circular-dependency)
                                      %)
                                   is)))))))

(defn- differing-names
  "Names whose `:tokens`, `:resolved`, `:issues` or `:graph` entry differs
  between the results `a` and `b`."
  [a b]
  (into #{}
        (filter (fn [name] (some #(not= (get-in a [% name]) (get-in b [% name])) [:tokens :resolved :issues :graph])))
        (concat (keys (:resolved a)) (keys (:resolved b)))))

(t/deftest incremental-operations-property-test
  (let [property
        (prop/for-all [sets gen-sets
                       ops (gen/vector gen-op 1 12)]
                      (let [cfg (h/core-config)]
                        (loop [prev (proc/build sets {:config cfg :token-writer vector}) ops ops]
                          (if-let [op (first ops)]
                            (if-let [res (apply-op (:state prev) op)]
                              (let [fresh (proc/build (get-in res [:state :sets]) {:config cfg})]
                                (if (and (= (snapshot (normalize-cycle-issues fresh))
                                            (snapshot (normalize-cycle-issues res)))
                                         (= (:graph fresh) (:graph res))
                                         (= (set (keys (:resolved fresh))) (set (keys (:resolved res))))
                                         (= (:output res)
                                            (into {} (map (fn [n] [n (vector (proc/input res n) (get-in res [:tokens n]) (get-in res [:issues n]))]))
                                                  (keys (:tokens res))))
                                         (every? (:changed res) (differing-names prev res)))
                                  (recur res (rest ops))
                                  false))
                              (recur prev (rest ops)))
                            true))))
        result (tc/quick-check 100 property :max-size 50)]
    (t/is (true? (:pass? result))
          (pr-str (select-keys result [:seed :shrunk])))))

(t/deftest resolve-value-test
  (let [st (:state (process {"a" "10" "b" "{a} + 5"}))]
    (t/is (= "16px" (sym/to-string (:value (proc/resolve-value st "{b} + 1px")))))
    (t/is (= [:interpreter/unknown-reference] (mapv :code (:issues (proc/resolve-value st "{zzz}")))))
    (t/is (= [:lexer/invalid-character] (mapv :code (:issues (proc/resolve-value st "1 ´ 2")))))
    (t/is (= {:value nil :issues []} (proc/resolve-value st "")))))

(t/deftest dependents-test
  (let [graph {"a" #{} "b" #{"a"} "c" #{"b"} "d" #{"a"} "e" #{}}
        {:keys [tokens subgraph]} (proc/dependents graph "a")]
    (t/is (= #{"a" "b" "c" "d"} tokens))
    (t/is (= {"a" #{} "b" #{"a"} "c" #{"b"} "d" #{"a"}} subgraph))))

(t/deftest outputs-test
  (let [res (process {"a" "8px" "c" "#fff" "l" "1, 2"})]
    (t/is (= {"a" "8px" "c" "#fff" "l" "1, 2"} (proc/tokens->strings res)))
    (t/is (= {"a" {:value 8.0 :unit "px"} "c" {:type "Hex" :value "#fff" :alpha nil} "l" [1.0 2.0]}
             (-> (proc/tokens->data res)
                 (update-in ["a" :value] double)
                 (update "l" #(mapv double %)))))))

(t/deftest facade-test
  (let [res (tsc/build {"a" "lighten(red, 0.1)"})]
    (t/is (= "Color.Oklch" (sym/full-type-name (get-in res [:tokens "a"]))))
    (t/is (= "Color.Hex" (sym/full-type-name (tsc/color->hex (get-in res [:tokens "a"])))))))

(t/deftest long-reference-chains-test
  ;; Resolution recurses per reference: node overflows its stack at about
  ;; 20000 links, far beyond real token files.
  (let [n 1000
        tokens (into {"t0" "1"} (map (fn [i] [(str "t" i) (str "{t" (dec i) "} + 1")])) (range 1 n))]
    (t/is (= (str n) (value (process tokens) (str "t" (dec n)))))))