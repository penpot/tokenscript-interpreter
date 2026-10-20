;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns tokenscript.document.themes
  "The sets and themes of multi-set token files: `$themes` lists the
  themes, `$metadata` orders the sets and names the active themes and
  sets.

  See docs/dtcg.md."
  (:require
   [tokenscript.document.dtcg :as dtcg]))

(defn multi-set?
  "A file with `$themes` or `$metadata` holds a set under each of its
  other keys."
  [document]
  (or (contains? document "$themes") (contains? document "$metadata")))

;; Reading --------------------------------------------------------------------

(defn- read-status
  [status]
  (case status
    "enabled" :enabled
    "source" :source
    "disabled" :disabled
    nil))

(defn- read-selected-sets
  "Files write `selectedTokenSets` as `{name status}` or as
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

(defn read-themes
  [document]
  (let [themes (get document "$themes")]
    (if (sequential? themes) (into [] (keep read-theme) themes) [])))

(defn- metadata
  [document]
  (let [metadata (get document "$metadata")]
    (if (map? metadata) metadata {})))

(defn- names
  "A list of names from `$metadata`, also as a set when the document
  comes from `write` without being encoded. Some files have an empty
  object instead of an empty list."
  [v]
  (when (coll? v) (vec (filter string? v))))

(defn set-names
  "The sets of a multi-set file in the order of `$metadata`, followed by
  the sets it doesn't list."
  [document]
  (let [present (into #{} (comp (map key) (remove dtcg/property?) (filter #(map? (get document %)))) document)
        order (filter present (names (get (metadata document) "tokenSetOrder")))]
    (distinct (concat order (filter present (map key document))))))

(defn read-activation
  "The themes and sets `$metadata` names as active. A list the file
  doesn't have stays out, rather than empty."
  [document]
  (let [metadata (metadata document)]
    (cond-> {}
      (contains? metadata "activeThemes") (assoc :active-themes (or (names (get metadata "activeThemes")) []))
      (contains? metadata "activeSets") (assoc :active-sets (or (names (get metadata "activeSets")) [])))))

;; Writing --------------------------------------------------------------------

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

(defn write-themes
  [themes]
  (mapv write-theme themes))

(defn write-metadata
  [{:keys [sets active-themes active-sets]}]
  {"tokenSetOrder" (mapv :name sets)
   "activeThemes" (or active-themes [])
   "activeSets" (or active-sets [])})
