;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns tokenscript.dtcg
  "Reads and writes design token files: DTCG (the 2022 drafts and the
  Format Module 2025.10), Tokens Studio's legacy `value`/`type` files,
  and Tokens Studio's multi-set files with `$themes` and `$metadata`.

  Documents are decoded JSON (maps with string keys) on both sides;
  decoding and encoding the text is the caller's. A document reads into
  plain data:

    {:document      :single-set | :multi-set
     :sets          [{:name \"core\"
                      :tokens {\"color.primary\" {:value \"#fff\" :type \"color\" ..}}
                      :groups {\"color\" {:description \"..\" ..}}}]
     :themes        [{:id .. :name .. :group .. :sets {\"core\" :enabled}}]
     :active-themes [\"group/name\"]
     :active-sets   [\"core\"]
     :issues        [{:type :tokenscript/issue :code :dtcg/missing-type ..}]}

  Token data has the `:value` and `:type` keys the processor reads, so
  the sets of a document go into `tokenscript.core/build` as they are.

  See docs/dtcg.md."
  (:refer-clojure :exclude [read])
  (:require
   [clojure.string :as str]
   [tokenscript.dtcg.values :as values]
   [tokenscript.errors :as err]))

(def ^:private reference-re #"^\{([^{}]+)\}$")

(defn- issue
  [code severity data]
  {:type :tokenscript/issue
   :code code
   :severity severity
   :data data})

(defn- join-path
  [prefix k]
  (if (str/blank? prefix) k (str prefix "." k)))

;; Token nodes ----------------------------------------------------------------

(defn- property?
  [k]
  (and (string? k) (str/starts-with? k "$")))

(defn- legacy-token?
  "Tokens Studio's files before DTCG: `value` and `type` without `$`."
  [node]
  (and (contains? node "value") (contains? node "type") (not (contains? node "$value"))))

(defn- token-node?
  "A node is a token when it has a `$value` (DTCG), a `$ref` (a JSON
  pointer alias) or `value` and `type` (legacy). Every other map is a
  group."
  [node]
  (and (map? node)
       (or (contains? node "$value")
           (contains? node "$ref")
           (legacy-token? node))))

(defn- pointer->reference
  "`#/colors/blue/$value` as `{colors.blue}`. Pointers into a value
  (`#/colors/blue/$value/components/0`) are not supported."
  [pointer]
  (when-let [[_ path] (and (string? pointer) (re-matches #"^#/(.+)/\$value$" pointer))]
    (str "{" (->> (str/split path #"/")
                  (map #(-> % (str/replace "~1" "/") (str/replace "~0" "~")))
                  (str/join "."))
         "}")))

(def ^:private known-token-keys
  #{"$value" "$type" "$description" "$deprecated" "$extensions" "$ref"})

(defn- read-token-node
  "Token data of a token node, or `{:error ..}` for a value that cannot
  be read. `type` is the inherited group type. With
  `original-type-namespace`, a type kept there by `write` wins and the
  namespace is taken out of the extensions."
  [node type {:keys [original-type-namespace]}]
  (let [legacy? (legacy-token? node)
        extensions (get node "$extensions")
        original-type (when original-type-namespace
                        (get-in extensions [original-type-namespace "type"]))
        extensions (cond-> extensions
                     (and original-type-namespace (map? extensions)) (dissoc original-type-namespace))
        raw-value (cond
                    legacy? (get node "value")
                    (contains? node "$value") (get node "$value")
                    :else (pointer->reference (get node "$ref")))
        type (or original-type (get node "$type") (when legacy? (get node "type")) type)
        type (some-> type values/read-type)
        description (or (get node "$description") (when legacy? (get node "description")))
        properties (not-empty (into {} (remove (fn [[k]] (or (contains? known-token-keys k)
                                                             (not (property? k)))))
                                    node))]
    (if (and (contains? node "$ref") (not (contains? node "$value")) (nil? raw-value))
      {:error {:reason :unsupported-pointer :pointer (get node "$ref")}}
      (let [{:keys [value error]} (values/read-value type raw-value)]
        (if error
          {:error error}
          (cond-> {:value value}
            (some? type) (assoc :type type)
            (string? description) (assoc :description description)
            (contains? node "$deprecated") (assoc :deprecated (get node "$deprecated"))
            (and (map? extensions) (seq extensions)) (assoc :extensions extensions)
            properties (assoc :properties properties)))))))

;; Groups ---------------------------------------------------------------------

(defn- lookup-group
  [tree path]
  (get-in tree (str/split path #"\.")))

(defn- deep-merge-group
  "`local` over `base`: tokens are replaced whole, groups are merged."
  [base local]
  (reduce-kv (fn [acc k v]
               (let [b (get acc k)]
                 (assoc acc k (if (and (map? b) (map? v) (not (token-node? b)) (not (token-node? v)))
                                (deep-merge-group b v)
                                v))))
             base
             local))

(defn- expand-extends
  "Replaces every `$extends` of a group in `tree` by the tokens of the
  group it names, the local tokens winning (DTCG 2025.10, 6.4)."
  [tree]
  (let [issues (volatile! [])
        expand (fn expand [node path visiting]
                 (if (or (not (map? node)) (token-node? node))
                   node
                   (let [target (when-let [[_ ref] (and (string? (get node "$extends"))
                                                        (re-matches reference-re (get node "$extends")))]
                                  ref)
                         base (when target
                                (cond
                                  (contains? visiting target)
                                  (do (vswap! issues conj (issue :dtcg/circular-extends :error {:group path :extends target}))
                                      nil)

                                  (map? (lookup-group tree target))
                                  (expand (lookup-group tree target) target (conj visiting path))

                                  :else
                                  (do (vswap! issues conj (issue :dtcg/invalid-extends :error {:group path :extends target}))
                                      nil)))
                         node (cond-> (dissoc node "$extends")
                                (map? base) (->> (deep-merge-group (dissoc base "$extends"))))]
                     (reduce-kv (fn [acc k v]
                                  (assoc acc k (if (property? k) v (expand v (join-path path k) (conj visiting path)))))
                                node
                                node))))]
    [(expand tree "" #{}) @issues]))

(def ^:private group-property-keys
  {"$description" :description
   "$deprecated" :deprecated
   "$extensions" :extensions})

(defn- group-data
  "The properties of a group other than `$type`, which is inherited by
  its tokens."
  [node]
  (let [props (reduce-kv (fn [acc k v]
                           (cond
                             (= "$type" k) acc
                             (contains? group-property-keys k) (assoc acc (get group-property-keys k) v)
                             (property? k) (assoc-in acc [:properties k] v)
                             :else acc))
                         {}
                         node)]
    (not-empty props)))

;; Sets -----------------------------------------------------------------------

(defn- flatten-set
  "Reads the token tree of one set into `{:tokens :groups :issues}`.
  Group `$type` is inherited, `$root` is the token of its group's name
  (DTCG 2025.10, 6.2)."
  [set-name tree opts]
  (let [[tree extends-issues] (expand-extends tree)
        acc (volatile! {:tokens {} :groups {} :issues (mapv #(assoc % :set set-name) extends-issues)})
        add-issue (fn [code severity data]
                    (vswap! acc update :issues conj (assoc (issue code severity data) :set set-name)))
        walk (fn walk [node path type]
               (let [type (or (get node "$type") type)]
                 (when-let [data (group-data node)]
                   (when-not (str/blank? path)
                     (vswap! acc assoc-in [:groups path] data)))
                 (doseq [[k child] node
                         :when (not= "$extends" k)]
                   (cond
                     (and (= "$root" k) (token-node? child))
                     (if (str/blank? path)
                       (add-issue :dtcg/invalid-name :error {:token-name "$root"})
                       (vswap! acc assoc-in [:tokens path] [child type]))

                     (property? k)
                     nil

                     (token-node? child)
                     (vswap! acc assoc-in [:tokens (join-path path k)] [child type])

                     (map? child)
                     (walk child (join-path path k) type)

                     :else
                     (add-issue :dtcg/invalid-node :warning {:token-name (join-path path k)})))))]
    (walk tree "" nil)
    (let [{:keys [tokens] :as result} @acc
          tokens (reduce-kv (fn [tokens token-name [node type]]
                              (let [{:keys [error] :as data} (read-token-node node type opts)]
                                (if error
                                  (do (add-issue :dtcg/invalid-value :error (assoc error :token-name token-name))
                                      tokens)
                                  (assoc tokens token-name data))))
                            {}
                            tokens)]
      (assoc result :tokens tokens :issues (:issues @acc)))))

(defn- name-collisions
  "Issues for token names that are a group of another token's name in
  the same set (`a` and `a.b`)."
  [set-name tokens]
  (let [names (sort (keys tokens))]
    (->> (map vector names (rest names))
         (keep (fn [[a b]]
                 (when (str/starts-with? b (str a "."))
                   (assoc (issue :dtcg/name-collision :warning {:token-name a :group-of b}) :set set-name)))))))

(defn- reference-target
  [value]
  (when (string? value)
    (second (re-matches reference-re (str/trim value)))))

(defn- infer-types
  "Types of the tokens without one that are a single reference: the type
  of the referenced token (DTCG 2025.10, 5.2.2), looked up across sets,
  the last set winning. Tokens left without a type are dropped with
  `:dtcg/missing-type`."
  [sets]
  (let [types (reduce (fn [acc {:keys [tokens]}]
                        (reduce-kv (fn [acc n {:keys [type value]}]
                                     (assoc acc n {:type type :ref (reference-target value)}))
                                   acc
                                   tokens))
                      {}
                      sets)
        type-of (fn type-of [n seen]
                  (let [{:keys [type ref]} (get types n)]
                    (cond
                      (some? type) type
                      (and ref (not (contains? seen ref))) (type-of ref (conj seen n))
                      :else nil)))]
    (mapv (fn [{:keys [name tokens] :as set}]
            (let [[tokens issues]
                  (reduce-kv (fn [[tokens issues] n data]
                               (if (some? (:type data))
                                 [(assoc tokens n data) issues]
                                 (if-let [type (type-of n #{})]
                                   [(assoc tokens n (assoc data :type type)) issues]
                                   [tokens (conj issues (assoc (issue :dtcg/missing-type :warning {:token-name n}) :set name))])))
                             [{} []]
                             tokens)]
              (-> set
                  (assoc :tokens tokens)
                  (update :issues into issues))))
          sets)))

(defn- read-sets
  [named-trees opts]
  (->> named-trees
       (map (fn [[set-name tree]]
              (let [{:keys [tokens] :as result} (flatten-set set-name tree opts)]
                (-> (assoc result :name set-name)
                    (update :issues into (name-collisions set-name tokens))))))
       (infer-types)))

;; Themes and metadata --------------------------------------------------------

(defn- read-status
  [status]
  (case status
    "enabled" :enabled
    "source" :source
    "disabled" :disabled
    nil))

(defn- read-selected-sets
  "Tokens Studio writes `selectedTokenSets` as `{name status}` or as
  `[{id status}]`."
  [selected]
  (cond
    (map? selected)
    (into {} (keep (fn [[k v]] (when-let [s (read-status v)] [k s]))) selected)

    (sequential? selected)
    (into {} (keep (fn [m] (when-let [s (and (map? m) (read-status (get m "status")))]
                             [(get m "id") s])))
          selected)

    :else {}))

(defn- read-theme
  [theme]
  (when (map? theme)
    (cond-> {:name (get theme "name")
             :sets (read-selected-sets (get theme "selectedTokenSets"))
             :is-source (boolean (or (get theme "isSource") (get theme "is-source")))}
      (string? (get theme "id")) (assoc :id (get theme "id"))
      (string? (get theme "group")) (assoc :group (get theme "group"))
      (string? (get theme "description")) (assoc :description (get theme "description")))))

(defn- names
  "A list of names from `$metadata`; Tokens Studio sometimes writes an
  empty object instead of an empty list."
  [v]
  (when (sequential? v) (vec (filter string? v))))

(defn- ordered-set-names
  [document metadata]
  (let [present (into #{} (comp (map key) (remove property?) (filter #(map? (get document %)))) document)
        order (filter present (names (get metadata "tokenSetOrder")))]
    (distinct (concat order (filter present (map key document))))))

;; Reading --------------------------------------------------------------------

(defn read
  "Reads a decoded token file. Options:

    :set-name                 the set name of a single set file
    :multi-set?               read the document as sets even without
                              `$themes` and `$metadata`
    :original-type-namespace  see `write`

  Throws `:dtcg/invalid-document` when `document` is not a JSON object;
  everything else is an issue."
  ([document] (read document nil))
  ([document {:keys [set-name multi-set?] :as opts}]
   (when-not (map? document)
     (err/raise :dtcg/invalid-document {:data {:type (cond (nil? document) "null"
                                                           (sequential? document) "array"
                                                           :else "value")}}))
   (if-not (or multi-set? (contains? document "$themes") (contains? document "$metadata"))
     (let [sets (read-sets [[set-name document]] opts)]
       {:document :single-set
        :sets sets
        :themes []
        :issues (vec (mapcat :issues sets))})
     (let [metadata (get document "$metadata")
           metadata (if (map? metadata) metadata {})
           sets (read-sets (map (fn [n] [n (get document n)]) (ordered-set-names document metadata)) opts)
           themes (let [ts (get document "$themes")]
                    (if (sequential? ts) (into [] (keep read-theme) ts) []))]
       (cond-> {:document :multi-set
                :sets sets
                :themes themes
                :issues (vec (mapcat :issues sets))}
         (contains? metadata "activeThemes") (assoc :active-themes (or (names (get metadata "activeThemes")) []))
         (contains? metadata "activeSets") (assoc :active-sets (or (names (get metadata "activeSets")) [])))))))

;; Writing --------------------------------------------------------------------

(defn- write-token
  [{:keys [value type description deprecated]} {:keys [profile original-type-namespace]}]
  (let [dtcg? (= :dtcg profile)
        file-type (cond-> type (and dtcg? (some? type)) values/write-type)]
    (cond-> {"$value" (if dtcg? (values/write-value type value) value)}
      (some? file-type) (assoc "$type" file-type)
      (some? description) (assoc "$description" description)
      (and dtcg? (some? deprecated)) (assoc "$deprecated" deprecated)
      (and dtcg? original-type-namespace (not= type file-type))
      (assoc "$extensions" {original-type-namespace {"type" type}}))))

(defn- assoc-token
  "Puts a token into the nested tree at its name. A token whose name is
  also a group goes under `$root` (DTCG 2025.10, 6.2)."
  [tree path token]
  (let [[k & more] path
        node (get tree k)]
    (if (seq more)
      (assoc tree k (assoc-token (if (and (map? node) (not (contains? node "$value")))
                                   node
                                   (cond-> {} (map? node) (assoc "$root" node)))
                                 more token))
      (assoc tree k (if (and (map? node) (not (contains? node "$value")))
                      (assoc node "$root" token)
                      token)))))

(defn- write-tokens
  [tokens opts]
  (reduce (fn [tree [token-name data]]
            (assoc-token tree (str/split token-name #"\.") (write-token data opts)))
          {}
          (sort-by key tokens)))

(defn- write-status
  [status]
  (name status))

(defn- write-theme
  [{:keys [id name group description is-source sets]}]
  (cond-> {"name" name
           "selectedTokenSets" (into {} (map (fn [[k v]] [k (write-status v)])) sets)}
    (some? id) (assoc "id" id)
    (some? group) (assoc "group" group)
    (some? description) (assoc "description" description)
    (some? is-source) (assoc "isSource" is-source)))

(defn- write-metadata
  [{:keys [sets active-themes active-sets]}]
  {"tokenSetOrder" (mapv :name sets)
   "activeThemes" active-themes
   "activeSets" active-sets})

(defn write
  "Writes a document (the shape `read` returns) as one decoded multi-set
  token file. Options:

    :profile                  `:tokens-studio` (default): values as
                              given, Tokens Studio type names. `:dtcg`:
                              DTCG 2025.10 values and type names.
    :original-type-namespace  with `:dtcg`, an `$extensions` namespace
                              to keep the Tokens Studio type in when the
                              DTCG type differs (`sizing` is written as
                              `dimension`); `read` with the same option
                              restores it."
  ([document] (write document nil))
  ([{:keys [sets themes] :as document} opts]
   (-> (into {} (map (fn [{:keys [name tokens]}] [name (write-tokens tokens opts)])) sets)
       (assoc "$themes" (mapv write-theme themes))
       (assoc "$metadata" (write-metadata document)))))

(defn write-files
  "Like `write`, as one decoded file per set, `$themes.json` and
  `$metadata.json`, keyed by file name."
  ([document] (write-files document nil))
  ([{:keys [sets themes] :as document} opts]
   (-> (into {} (map (fn [{:keys [name tokens]}] [(str name ".json") (write-tokens tokens opts)])) sets)
       (assoc "$themes.json" (mapv write-theme themes))
       (assoc "$metadata.json" (write-metadata document)))))
