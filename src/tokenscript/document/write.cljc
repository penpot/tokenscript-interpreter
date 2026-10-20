;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL


(ns tokenscript.document.write
  "Writes documents, the data `tokenscript.document.read/read` gives, as
  multi-set files: one with every set, `$themes` and `$metadata`, or a
  file for each.

  See docs/dtcg.md."
  (:require
   [clojure.string :as str]
   [tokenscript.document.dtcg :as dtcg]
   [tokenscript.document.themes :as themes]))

(defn- write-token
  [{:keys [value type description deprecated]} {:keys [profile]}]
  (let [dtcg? (= :dtcg profile)
        file-type (cond-> type (and dtcg? (some? type)) dtcg/write-type)]
    (cond-> {"$value" (if dtcg? (dtcg/write-value type value) value)}
      (some? file-type) (assoc "$type" file-type)
      (some? description) (assoc "$description" description)
      (and dtcg? (some? deprecated)) (assoc "$deprecated" deprecated))))

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

(defn write
  "Writes a document as one decoded multi-set token file. Options:

    :profile  `:default`: values and type names as given. `:dtcg`:
              DTCG 2025.10 values and type names."
  ([document] (write document nil))
  ([{:keys [sets themes] :as document} opts]
   (-> (into {} (map (fn [{:keys [name tokens]}] [name (write-tokens tokens opts)])) sets)
       (assoc "$themes" (themes/write-themes themes))
       (assoc "$metadata" (themes/write-metadata document)))))

(defn write-files
  "Like `write`, as one decoded file per set, `$themes.json` and
  `$metadata.json`, keyed by file name."
  ([document] (write-files document nil))
  ([{:keys [sets themes] :as document} opts]
   (-> (into {} (map (fn [{:keys [name tokens]}] [(str name ".json") (write-tokens tokens opts)])) sets)
       (assoc "$themes.json" (themes/write-themes themes))
       (assoc "$metadata.json" (themes/write-metadata document)))))
