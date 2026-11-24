;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns tokenscript.importers.syntax
  "Reads the CSS syntax that the importers share. It splits the text into
  tokens: numbers with their unit, names, hex colors, token references
  and punctuation. It parses each call with its arguments, nested as in
  CSS.

  It does no arithmetic. So `rgb(255 0 0 / 50%)` keeps its slash, and
  `red -20%` stays a name and a number. Tokenscript would read these as
  a division and a subtraction. The importer rewrites them.

  Each importer adds its own entry point and its own rewrite rules."
  (:require
   [clojure.string :as str]))

;; Tokenizer ------------------------------------------------------------------

(defn code-at
  "Gives the character code at `index` of `text`."
  [text index]
  #?(:clj  (int (.charAt ^String text (int index)))
     :cljs (.charCodeAt ^string text index)))

(defn- digit?
  [code]
  (<= 48 code 57))

(defn- letter?
  [code]
  (or (<= 65 code 90) (<= 97 code 122)))

(defn hex-digit?
  "Checks whether the character code is a hexadecimal digit."
  [code]
  (or (digit? code) (<= 65 code 70) (<= 97 code 102)))

(defn- space?
  [code]
  (or (= 32 code) (<= 9 code 13)))

(defn- name-char?
  [code]
  (or (letter? code) (digit? code) (= 45 code) (= 95 code)))

(defn- skip-while
  "Gives the first index from `start` where `pred?` is false for the
  character."
  [text start pred?]
  (loop [index start]
    (if (and (< index (count text)) (pred? (code-at text index)))
      (recur (inc index))
      index)))

(defn- number-start?
  "Checks whether a number starts at `index`. A number starts with
  digits, or with a dot or a sign before digits."
  [text index]
  (let [code   (code-at text index)
        second (when (< (inc index) (count text)) (code-at text (inc index)))
        third  (when (< (+ index 2) (count text)) (code-at text (+ index 2)))]
    (or (digit? code)
        (and (= 46 code) (some? second) (digit? second))
        (and (or (= 43 code) (= 45 code))
             (some? second)
             (or (digit? second)
                 (and (= 46 second) (some? third) (digit? third)))))))

(defn- read-number
  "Reads the number at `index` and its unit, if it has one (`50%`,
  `90deg`)."
  [text index]
  (let [sign-end   (if (or (= 43 (code-at text index)) (= 45 (code-at text index)))
                     (inc index)
                     index)
        int-end    (skip-while text sign-end digit?)
        has-dot?   (and (< (inc int-end) (count text))
                        (= 46 (code-at text int-end))
                        (digit? (code-at text (inc int-end))))
        number-end (if has-dot?
                     (skip-while text (inc int-end) digit?)
                     int-end)
        unit-end   (if (and (< number-end (count text)) (= 37 (code-at text number-end)))
                     (inc number-end)
                     (skip-while text number-end letter?))
        unit       (subs text number-end unit-end)]
    {:type  :number
     :value (parse-double (subs text index number-end))
     :unit  (when-not (str/blank? unit) (str/lower-case unit))
     :start index
     :end   unit-end}))

(defn- read-reference
  "Reads the token reference at `index`, up to its closing brace. Gives
  nil when the reference has no closing brace."
  [text index]
  (loop [position (inc index)
         depth    1]
    (when (< position (count text))
      (let [code (code-at text position)]
        (cond
          (= 123 code) (recur (inc position) (inc depth))
          (= 125 code) (if (= 1 depth)
                         {:type :reference :start index :end (inc position)}
                         (recur (inc position) (dec depth)))
          :else        (recur (inc position) depth))))))

(defn- read-token
  "Reads the token at `index`. Gives nil when the text there cannot be a
  token."
  [text index]
  (let [code (code-at text index)]
    (cond
      (number-start? text index)
      (read-number text index)

      (or (letter? code) (= 95 code) (= 45 code))
      {:type :name :start index :end (skip-while text index name-char?)}

      (= 35 code)
      {:type :hash :start index :end (skip-while text (inc index) hex-digit?)}

      (= 123 code)
      (read-reference text index)

      ;; A quoted string, as in the `filter` of old gradient CSS. Nil when
      ;; it is never closed.
      (or (= 34 code) (= 39 code))
      (when-let [end (str/index-of text (subs text index (inc index)) (inc index))]
        {:type :string :start index :end (inc end)})

      ;; Escapes are not part of the values the importers read
      (= 92 code)
      nil

      :else
      {:type :punctuation :start index :end (inc index)})))

(defn tokenize
  "Splits `text` into tokens. Each token keeps its range in `text`. Gives
  nil when `text` contains something that the importers do not read."
  [text]
  (loop [index  0
         tokens []]
    (let [index (skip-while text index space?)]
      (cond
        (>= index (count text))
        tokens

        ;; A comment is skipped, CSS copied from a tool has them
        (str/starts-with? (subs text index) "/*")
        (when-let [end (str/index-of text "*/" (+ index 2))]
          (recur (+ end 2) tokens))

        :else
        (when-let [token (read-token text index)]
          (recur (:end token)
                 (conj tokens (assoc token :text (subs text (:start token) (:end token))))))))))

;; Parser ---------------------------------------------------------------------

(defn punctuation?
  "Checks whether `token` is the punctuation mark `text`."
  [token text]
  (and (= :punctuation (:type token))
       (= text (:text token))))

(defn parse-value
  "Parses the value at `index`: a call with its arguments, or a single
  token. Gives the value and the index after it. Gives nil when the
  tokens are not a value."
  [text tokens index]
  (let [token (get tokens index)
        open  (get tokens (inc index))]
    (cond
      (nil? token)
      nil

      ;; A call, with the values of each of its arguments
      (and (= :name (:type token)) (some? open) (punctuation? open "("))
      (loop [position (+ index 2)
             current  []
             args     []]
        (when-let [argument (get tokens position)]
          (cond
            (punctuation? argument ")")
            [{:type :call
              :name (str/lower-case (:text token))
              :args (conj args current)
              :text (subs text (:start token) (:end argument))}
             (inc position)]

            (punctuation? argument ",")
            (recur (inc position) [] (conj args current))

            :else
            (when-let [[value next-position] (parse-value text tokens position)]
              (recur next-position (conj current value) args)))))

      (or (punctuation? token "(") (punctuation? token ")") (punctuation? token ","))
      nil

      :else
      [token (inc index)])))