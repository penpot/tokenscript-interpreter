;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL


(ns tokenscript.document.read
  "Reads design token files: DTCG (the 2022 drafts and the Format Module
  2025.10), legacy files with `value` and `type` without `$`, and
  multi-set files with `$themes` and `$metadata`. The format is decided
  per token, so files that mix them read.

  A file is decoded JSON (maps with string keys); decoding the text is
  the caller's. It reads into plain data, a document:

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
   [tokenscript.document.dtcg :as dtcg]
   [tokenscript.document.themes :as themes]
   [tokenscript.errors :as err]))

(def ^:private reference-re #"^\{([^{}]+)\}$")

(defn- reference-target
  [value]
  (when (string? value)
    (second (re-matches reference-re (str/trim value)))))

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

(defn- token-node?
  "A DTCG or a legacy token. Every other map is a group."
  [node]
  (or (dtcg/token? node) (dtcg/legacy-token? node)))

(defn- group-node?
  [node]
  (and (map? node) (not (token-node? node))))

(def ^:private known-token-keys
  #{"$value" "$type" "$description" "$deprecated" "$extensions" "$ref"})

(defn- read-token-node
  "`{:token data :warnings ..}` of a token node, or `{:error ..}` for a
  value that cannot be read. `type` is the inherited group type."
  [node type]
  (let [legacy? (dtcg/legacy-token? node)
        extensions (get node "$extensions")
        raw-value (cond
                    legacy? (get node "value")
                    (contains? node "$value") (get node "$value")
                    :else (dtcg/pointer->reference (get node "$ref")))
        type (or (get node "$type") (when legacy? (get node "type")) type)
        type (when (string? type) (dtcg/read-type type))
        description (or (get node "$description") (when legacy? (get node "description")))
        properties (not-empty (into {} (remove (fn [[k]] (or (contains? known-token-keys k)
                                                             (not (dtcg/property? k)))))
                                    node))]
    (if (and (contains? node "$ref") (not (contains? node "$value")) (nil? raw-value))
      {:error {:reason :unsupported-pointer :pointer (get node "$ref")}}
      (let [{:keys [value error warnings]} (dtcg/read-value type raw-value)]
        (if error
          {:error error}
          {:token (cond-> {:value value}
                    (some? type) (assoc :type type)
                    (string? description) (assoc :description description)
                    (contains? node "$deprecated") (assoc :deprecated (get node "$deprecated"))
                    (and (map? extensions) (seq extensions)) (assoc :extensions extensions)
                    properties (assoc :properties properties))
           :warnings warnings})))))

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

(defn- group-ref?
  "A `$ref` of a group is the JSON Schema form of `$extends` (DTCG
  2025.10, 6.4.1): on a group with tokens of its own, or pointing at a
  group."
  [tree node]
  (let [pointer (get node "$ref")
        target (dtcg/pointer->path pointer)]
    (and (map? node)
         (some? target)
         (not (contains? node "$value"))
         (not (str/ends-with? pointer "/$value"))
         (or (group-node? node)
             (group-node? (lookup-group tree target))))))

(defn- group-refs->extends
  [tree]
  (let [walk (fn walk [node]
               (cond
                 (group-ref? tree node)
                 (walk (-> (dissoc node "$ref")
                           (assoc "$extends" (dtcg/pointer->reference (get node "$ref")))))

                 (group-node? node)
                 (reduce-kv (fn [acc k v] (cond-> acc (not (dtcg/property? k)) (assoc k (walk v))))
                            node
                            node)

                 :else node))]
    (walk tree)))

(defn- expand-extends
  "Replaces every `$extends` of a group in `tree` by the tokens of the
  group it names, the local tokens winning (DTCG 2025.10, 6.4)."
  [tree]
  (let [tree (group-refs->extends tree)
        issues (volatile! [])
        expand (fn expand [node path visiting]
                 (if-not (group-node? node)
                   node
                   (let [extends (get node "$extends")
                         target (reference-target extends)
                         invalid (fn [code]
                                   (vswap! issues conj (issue code :error {:group path :extends extends}))
                                   nil)
                         base (cond
                                (nil? extends) nil
                                (nil? target) (invalid :dtcg/invalid-extends)
                                (contains? visiting target) (invalid :dtcg/circular-extends)
                                (group-node? (lookup-group tree target)) (expand (lookup-group tree target) target (conj visiting path))
                                :else (invalid :dtcg/invalid-extends))
                         node (cond-> (dissoc node "$extends")
                                (map? base) (->> (deep-merge-group (dissoc base "$extends"))))]
                     (reduce-kv (fn [acc k v]
                                  (assoc acc k (if (dtcg/property? k) v (expand v (join-path path k) (conj visiting path)))))
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
                             (contains? #{"$type" "$root"} k) acc
                             (contains? group-property-keys k) (assoc acc (get group-property-keys k) v)
                             (dtcg/property? k) (assoc-in acc [:properties k] v)
                             :else acc))
                         {}
                         node)]
    (not-empty props)))

;; Sets -----------------------------------------------------------------------

(defn- flatten-set
  "Reads the token tree of one set into `{:tokens :groups :issues}`.
  Group `$type` is inherited, `$root` is the token of its group's name
  (DTCG 2025.10, 6.2)."
  [set-name tree]
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

                     (dtcg/property? k)
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
                              (let [{:keys [error token warnings]} (read-token-node node type)]
                                (doseq [{:keys [code] :as warning} warnings]
                                  (add-issue code :warning (-> warning (dissoc :code) (assoc :token-name token-name))))
                                (if error
                                  (do (add-issue :dtcg/invalid-value :error (assoc error :token-name token-name))
                                      tokens)
                                  (assoc tokens token-name token))))
                            {}
                            tokens)]
      (assoc result :tokens tokens :issues (:issues @acc)))))

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
  [named-trees]
  (->> named-trees
       (map (fn [[set-name tree]]
              (assoc (flatten-set set-name tree) :name set-name)))
       (infer-types)))

;; Reading --------------------------------------------------------------------

(defn read
  "Reads a decoded token file. Options:

    :set-name    the set name of a single set file
    :multi-set?  read the document as sets even without `$themes` and
                 `$metadata`

  Throws `:dtcg/invalid-document` when `document` is not a JSON object;
  everything else is an issue."
  ([document] (read document nil))
  ([document {:keys [set-name multi-set?]}]
   (when-not (map? document)
     (err/raise :dtcg/invalid-document {:data {:type (cond (nil? document) "null"
                                                           (sequential? document) "array"
                                                           :else "value")}}))
   (if-not (or multi-set? (themes/multi-set? document))
     (let [sets (read-sets [[set-name document]])]
       {:document :single-set
        :sets sets
        :themes []
        :issues (vec (mapcat :issues sets))})
     (let [sets (read-sets (map (fn [n] [n (get document n)]) (themes/set-names document)))]
       (merge {:document :multi-set
               :sets sets
               :themes (themes/read-themes document)
               :issues (vec (mapcat :issues sets))}
              (themes/read-activation document))))))
