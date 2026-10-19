;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns tokenscript.schema-uri
  "Parsing and construction of tokenscript schema URIs.

  A schema URI is a base followed by a version segment:

    <base>/<version>[/]

  The base is opaque (`/api/v1/core/rgb-color`, `types/tokens/color`)
  and the version is a semantic version prefix (`0`, `0.1`, `0.1.2`) or
  `latest`, as in `@tokens-studio/schema-validation`. Versions are maps
  `{:major 0 :minor 1 :patch 2}` with minor/patch optional, or the
  keyword `:latest`.

  Schemas are never fetched, so the host of an absolute URI
  (`https://host/api/v1/...`, as written by other tokenscript tools) is
  not part of the identity: `normalize` drops it and the registries only
  hold path URIs."
  (:require
   [clojure.string :as str]))

(defn- parse-int
  [s]
  (when (re-matches #"\d+" s)
    #?(:clj (Long/parseLong s) :cljs (js/parseInt s 10))))

(defn parse-semver
  "`\"0.1.2\"` -> `{:major 0 :minor 1 :patch 2}`, nil when not numeric."
  [s]
  (let [parts (str/split s #"\.")
        nums (map parse-int parts)]
    (when (and (every? some? nums) (<= 1 (count nums) 3))
      (let [[major minor patch] nums]
        (cond-> {:major major}
          (some? minor) (assoc :minor minor)
          (some? patch) (assoc :patch patch))))))

(defn parse-version
  [s]
  (if (= s "latest") :latest (parse-semver s)))

(defn version->string
  [version]
  (cond
    (map? version)
    (let [{:keys [major minor patch]} version]
      (cond
        (some? patch) (str major "." minor "." patch)
        (some? minor) (str major "." minor)
        :else (str major)))
    :else "latest"))

(defn build-uri
  "Builds a schema URI from its base and version."
  ([base version] (build-uri base version false))
  ([base version trailing-slash?]
   (str base (when-not (= base "") "/") (version->string version) (when trailing-slash? "/"))))

(defn- normalize*
  [uri]
  (if-let [[_ path] (re-matches #"^[a-zA-Z][a-zA-Z0-9+.-]*://[^/]*(/.*)?$" uri)]
    (or path "/")
    uri))

;; Schema URIs come from a small set (the registered schemas and their
;; version candidates) and are normalized and parsed on every lookup, so
;; both are cached.
(def ^:private max-cached-uris 4096)

(defonce ^:private normalize-cache (atom {}))

(defn normalize
  "Path URI of a schema URI: `https://host/api/v1/core/x/0/` ->
  `/api/v1/core/x/0/`. Other values (path URIs, `$self`, nil) are
  returned as is."
  [uri]
  (if-not (string? uri)
    uri
    (let [m @normalize-cache]
      (if-let [v (get m uri)]
        v
        (let [v (normalize* uri)]
          (swap! normalize-cache (fn [m] (assoc (if (>= (count m) max-cached-uris) {} m) uri v)))
          v)))))

(defn- parse-uri*
  [uri]
  (when (string? uri)
    (let [uri (normalize uri)
          trailing-slash? (str/ends-with? uri "/")
          stripped (cond-> uri trailing-slash? (subs 0 (dec (count uri))))
          i (str/last-index-of stripped "/")
          base (if i (subs stripped 0 i) "")
          version (parse-version (if i (subs stripped (inc i)) stripped))]
      (when version
        {:base base :version version :trailing-slash? trailing-slash?}))))

(defonce ^:private uri-cache (atom {}))

(defn parse-uri
  "Parses a schema URI into `{:base .. :version .. :trailing-slash? ..}`,
  or nil when its last segment is not a version. The host of absolute
  URIs is ignored."
  [uri]
  (let [m @uri-cache]
    (if (contains? m uri)
      (get m uri)
      (let [v (parse-uri* uri)]
        (swap! uri-cache (fn [m] (assoc (if (>= (count m) max-cached-uris) {} m) uri v)))
        v))))

(defn base-uri
  "URI without the version segment: `/api/v1/core/x/0/` ->
  `/api/v1/core/x/`. URIs without a version are returned as is."
  [uri]
  (if-let [{:keys [base trailing-slash?]} (parse-uri uri)]
    (cond-> base trailing-slash? (str "/"))
    (normalize uri)))

(defn version-candidates
  "URIs to try when resolving `uri`, from most to least specific:
  `0.0.1` -> `[0.0.1 0.0 0 latest]`."
  [uri]
  (let [uri (normalize uri)
        {:keys [base version trailing-slash?]} (parse-uri uri)]
    (if (map? version)
      (let [{:keys [major minor patch]} version]
        (cond-> [uri]
          (some? patch) (conj (build-uri base {:major major :minor minor} trailing-slash?))
          (some? minor) (conj (build-uri base {:major major} trailing-slash?))
          :always (conj (build-uri base :latest trailing-slash?))))
      [uri])))

(defn compare-versions
  [a b]
  (let [c (compare (:major a) (:major b))]
    (if (not= 0 c)
      c
      (let [c (compare (or (:minor a) 0) (or (:minor b) 0))]
        (if (not= 0 c)
          c
          (compare (or (:patch a) 0) (or (:patch b) 0)))))))

(defn same-schema?
  "Whether two URIs identify the same schema ignoring the version."
  [a b]
  (= (base-uri a) (base-uri b)))
