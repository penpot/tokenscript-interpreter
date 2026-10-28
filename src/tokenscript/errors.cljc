;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns tokenscript.errors
  "Structured errors for the tokenscript interpreter.

  Every error raised by the lexer, parser, interpreter or processor is
  an `ex-info` whose data is an error map:

    {:type  :tokenscript/error
     :kind  :lexer | :parser | :interpreter | :processor
     :code  :lexer/invalid-character
     :data  {...}                            ; code specific data
     :line  1
     :token {...}}

  The processor stores the same maps as values in its issues map, so
  errors are plain data on both sides of the API. Errors carry no
  messages: callers render the code and its data in their own words and
  language. The code keyword namespace selects the kind. The message of
  the thrown ex-info is the code as the string identifier of the other
  tokenscript implementations (`INT_UNKNOWN_REFERENCE`).

  See docs/errors.md."
  (:require
   [clojure.string :as str]))

;; Codes ----------------------------------------------------------------------

(def ^:private kind-prefixes
  {"lexer" "LEXER"
   "parser" "PARSER"
   "interpreter" "INT"
   "operations" "OP"
   "symbols" "SYM"
   "functions" "FN"
   "config" "CFG"
   "processor" "PROC"
   "color" "COLOR"
   "unit" "UNIT"
   "dtcg" "DTCG"})

(defn- code->kind
  "Maps an error code to the error kind (the TS error class)."
  [code]
  (case (namespace code)
    "lexer" :lexer
    "parser" :parser
    "processor" :processor
    "dtcg" :dtcg
    :interpreter))

(defn- code->string
  "Maps an error code keyword to the cross-runtime string identifier,
  e.g. `:interpreter/unknown-reference` -> \"INT_UNKNOWN_REFERENCE\"."
  [code]
  (str (get kind-prefixes (namespace code))
       "_"
       (str/upper-case (str/replace (name code) "-" "_"))))

;; Construction ---------------------------------------------------------------

(defn error-map
  "Builds the error data map for `code`. Options:

    :data  code specific data
    :token the source token near the error
    :line  explicit line (defaults to the token line)"
  ([code] (error-map code nil))
  ([code {:keys [data token line]}]
   (let [line (or line (:line token))]
     (cond-> {:type :tokenscript/error
              :kind (code->kind code)
              :code code
              :data (or data {})}
       (some? line) (assoc :line line)
       (some? token) (assoc :token token)))))

(defn error
  "Creates (without throwing) the ex-info for `code`. Its message is the
  string identifier of the code; the error map is the ex-data."
  ([code] (error code nil))
  ([code opts]
   (ex-info (code->string code) (error-map code opts) (:cause opts))))

(defn raise
  ([code] (throw (error code nil)))
  ([code opts] (throw (error code opts))))

;; Inspection -----------------------------------------------------------------

(defn error-map?
  "True for error data maps (the `ex-data` of raised errors and the
  values stored in processor issue maps)."
  [v]
  (and (map? v) (= :tokenscript/error (:type v))))

(defn language-error?
  "True when `e` is an exception raised by this module."
  [e]
  (error-map? (ex-data e)))

(defn ->error-map
  "Returns the error map for either an exception or an error map."
  [e]
  (cond
    (error-map? e) e
    (language-error? e) (ex-data e)
    :else nil))

(defn cause-data
  "Data describing `e` as the cause of another error: `{:cause
  error-map}` for errors of this module, `{:error message}` for foreign
  exceptions."
  [e]
  (if-let [m (->error-map e)]
    {:cause m}
    {:error (if #?(:clj (instance? Throwable e) :cljs (instance? js/Error e))
              (ex-message e)
              (str e))}))
