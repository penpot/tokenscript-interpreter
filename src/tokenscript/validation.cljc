;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns tokenscript.validation
  "Token value validation against registered token type schemas, see
  docs/validation.md.

  A validation result is `{:valid? bool :error code :token-type type
  :path [..]}`; `:path` points at the nested field for failures inside
  structured values."
  (:require
   [clojure.string :as str]
   [tokenscript.errors :as err]
   [tokenscript.interpreter :as interp]
   [tokenscript.registry :as reg]
   [tokenscript.symbols :as sym]
   [tokenscript.token-types :as token-types]))

(def max-depth 10)

(defn- failure
  [error token-type path]
  (cond-> {:valid? false :error error :token-type token-type}
    (seq path) (assoc :path path)))

(defn- memoized
  "`(f)` cached under `[kind k]` in the cache of `registry`. The keys are
  type names and URIs: few, and looked up for every token."
  [registry kind k f]
  (let [cache (:cache registry)]
    (if-some [v (-> @cache (get kind) (get k))]
      v
      (let [v (f)]
        (swap! cache update kind #(assoc (if (< (count %) 1000) % {}) k v))
        v))))

(defn- type-info
  "`{:spec .. :validation ..}` of a token type, with nil values when it is
  not registered."
  [registry token-type]
  (memoized registry ::types token-type
            #(hash-map :spec (reg/spec-by-name registry token-type)
                       :validation (token-types/validation registry token-type))))

(defn- spec-at
  [registry url]
  (:spec (memoized registry ::urls url #(hash-map :spec (reg/spec registry url)))))

(defn- script-error
  "Error code of the validation script for `value`, nil when it is valid
  or the type has no script."
  [config {:keys [ast script]} value]
  (if (nil? ast)
    (str "VALIDATION_ERROR: invalid validation script: " script)
    (try
      (let [result (interp/interpret ast {:references {"input" value} :config config})]
        (cond
          (and (sym/boolean? result) (true? (:value result))) nil
          (sym/string? result) (:value result)
          :else "VALIDATION_INVALID_RESULT"))
      (catch #?(:clj Throwable :cljs :default) e
        (str "VALIDATION_ERROR: " (err/describe e))))))

;; Script results only depend on the type and the value. Scalar values are
;; immutable and repeat within a build and across builds, so their results
;; are kept in the registry's cache, which a new registration replaces.
;; Lists, dictionaries and tokens hold mutable content and are not kept.
(def ^:private max-cached-results 10000)

(defn- cacheable?
  [value]
  (or (sym/number? value) (sym/number-with-unit? value) (sym/string? value)
      (sym/boolean? value) (sym/null? value)
      ;; Hex colors; channel maps cost as much to hash as a script run
      (and (sym/color? value) (string? (sym/value value)))))

(defn- same-registries?
  [a b]
  (and (identical? (:colors a) (:colors b))
       (identical? (:units a) (:units b))
       (identical? (:functions a) (:functions b))
       (identical? (:inline-constants a) (:inline-constants b))
       (identical? (:language-options a) (:language-options b))))

(defn- same-context?
  "Whether `config` has the registries the cached results of `entry`
  were computed with: a script can also depend on them."
  [entry config]
  (and (some? entry)
       (or (identical? (:config entry) config)
           (same-registries? (:config entry) config))))

(def ^:private none
  "Sentinel of a missing cache entry."
  #?(:clj (Object.) :cljs (js-obj)))

(defn- value-key
  "`[kind x]` keys of a cacheable value: primitives, whose hashes are
  cheap, instead of the symbol records, which every build creates anew."
  [value]
  (cond
    (sym/number-with-unit? value) [(:unit value) (:value value)]
    (sym/number? value) [::number (:value value)]
    (sym/string? value) [::string (:value value)]
    (sym/color? value) [::hex (str (sym/value value) "/" (:alpha value))]
    :else [::other value]))

(defn- cached-script-error
  "`script-error`, cached in the token type registry by the type name and
  value, together with the rest of the config it was computed with."
  [config validation token-type value]
  (if-not (cacheable? value)
    (script-error config validation value)
    (let [cache (:cache (:tokens config))
          entry (::results @cache)
          results (if (same-context? entry config) (:results entry) {})
          [kind x] (value-key value)
          cached (-> results (get token-type) (get kind) (get x none))]
      (if-not (identical? none cached)
        cached
        (let [error (script-error config validation value)
              results (if (< (:count entry 0) max-cached-results) results {})]
          (swap! cache assoc ::results
                 {:config config
                  :results (assoc-in results [token-type kind x] error)
                  :count (if (identical? results (:results entry)) (inc (:count entry 0)) 1)})
          error)))))

(defn- run-script
  [config token-type value path]
  (let [{:keys [validation]} (type-info (:tokens config) token-type)]
    (if-let [error (when validation (cached-script-error config validation token-type value))]
      (failure error token-type path)
      {:valid? true :token-type token-type})))

(def ^:private property-keys
  "Memoized `property-key`: field names are few and repeat in every value."
  (atom {}))

(defn- property-key
  "Field names match ignoring case, `-` and `_`: `lineHeight` is also
  `line-height` and `line_height`."
  [k]
  (let [k (name k)]
    (or (get @property-keys k)
        (let [pk (-> k (str/lower-case) (str/replace #"[-_]" ""))]
          (swap! property-keys #(assoc (if (< (count %) 1000) % {}) k pk))
          pk))))

(defn- property-lookup
  "Function from a field name to its value in `m`, see `property-key`."
  [m]
  (let [index (delay (into {} (map (fn [[k v]] [(property-key k) v])) m))]
    (fn [prop-name]
      (or (get m prop-name)
          (get @index (property-key prop-name))))))

(defn- value-map
  [value]
  (cond
    (sym/dictionary? value) (sym/value value)
    (and (sym/token? value) (map? (sym/value value))) (sym/value value)
    :else nil))

(declare ^:private validate*)

(defn- validate-field
  "Failures of a field `value` of the token type at `url`."
  [config url value visited path depth]
  (if (contains? visited url)
    [(failure "CIRCULAR_REFERENCE" url path)]
    (if-let [{type-name :name :as referenced} (spec-at (:tokens config) url)]
      (if (and (cacheable? value) (nil? (:schema referenced)))
        ;; The common case, a scalar of a type without fields
        (let [{:keys [valid?] :as result} (run-script config type-name value path)]
          (if valid? [] [result]))
        (into [] (remove :valid?) (validate* config type-name value (conj visited url) path (inc depth))))
      [])))

(defn- validate-properties
  [config m properties visited path depth]
  (let [lookup (property-lookup m)]
    (reduce-kv (fn [results prop-name {:keys [type url]}]
                 (let [prop-name (name prop-name)
                       prop-value (when (and (= "token" type) url) (lookup prop-name))]
                   (if (some? prop-value)
                     (into results (validate-field config url prop-value visited (conj path prop-name) depth))
                     results)))
               []
               properties)))

(defn- validate-items
  [config value items-spec visited path depth]
  (if-not (sym/list? value)
    []
    (reduce (fn [results [i item]]
              (let [item-path (conj path i)]
                (cond-> results
                  (and (= "object" (:type items-spec)) (:properties items-spec))
                  (into (if-let [m (value-map item)]
                          (validate-properties config m (:properties items-spec) visited item-path (inc depth))
                          []))

                  (and (= "token" (:type items-spec)) (:url items-spec))
                  (into (validate-field config (:url items-spec) item visited item-path depth)))))
            []
            (map-indexed vector (sym/value value)))))

(defn- validate*
  [config token-type value visited path depth]
  (if (> depth max-depth)
    [(failure "MAX_VALIDATION_DEPTH_EXCEEDED" token-type path)]
    (let [top (run-script config token-type value path)]
      (if-not (:valid? top)
        [top]
        (let [schema (:schema (:spec (type-info (:tokens config) token-type)))
              results (cond
                        (nil? schema) []
                        (and (= "list" (:type schema)) (:items schema))
                        (validate-items config value (:items schema) visited path depth)
                        (nil? (:properties schema)) []
                        :else (if-let [m (value-map value)]
                                (validate-properties config m (:properties schema) visited path depth)
                                []))]
          (if (seq results)
            results
            [{:valid? true :token-type token-type}]))))))

(defn validate
  "Validates `value` (a symbol) as a `token-type`. Returns a vector of
  results; a single valid result when everything passes."
  [config token-type value]
  (validate* config token-type value #{} [] 0))

(defn- ->issue
  [token-name token-type {:keys [error path] :as result}]
  (let [nested? (seq path)]
    {:type :tokenscript/issue
     :code error
     :severity :warning
     :message (if nested?
                (str "Validation failed at " (str/join "." (map str path)) ": " error)
                (str "Token validation failed: " error))
     :token-name token-name
     :path path
     :data (if nested?
             {:token-type (:token-type result) :parent-token-type token-type}
             {:token-type token-type})}))

(defn type-issues
  "Validation issues for a resolved token value. Returns `[]` when the
  type has no registered spec or the value is not a symbol."
  [config token-name token-type value]
  (let [{:keys [spec validation]} (when (and config (sym/symbol? value))
                                    (type-info (:tokens config) token-type))]
    (cond
      (nil? spec)
      []

      ;; Most tokens: a scalar of a type without fields
      (and (cacheable? value) (nil? (:schema spec)))
      (if-let [error (when validation (cached-script-error config validation token-type value))]
        [(->issue token-name token-type {:error error :token-type token-type})]
        [])

      :else
      (let [to-validate (if (sym/token? value)
                          (let [inner (sym/value value)]
                            (if (map? inner)
                              (sym/dictionary inner)
                              (sym/list inner false)))
                          value)]
        (into []
              (keep (fn [{:keys [valid? error] :as result}]
                      (when (and (not valid?) error)
                        (->issue token-name token-type result))))
              (validate config token-type to-validate))))))
