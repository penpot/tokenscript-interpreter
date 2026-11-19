;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns tokenscript.importers.syntax
  "The CSS syntax the importers read: cuts the text into tokens (numbers
  with their unit, names, hex colors, token references, punctuation) and
  parses calls with their arguments, as CSS nests them. It does no
  arithmetic, so `rgb(255 0 0 / 50%)` keeps its slash and `red -20%` is
  a name and a number: what tokenscript would read as a division or a
  subtraction stays for the importer to rewrite.

  Each importer adds its own entry point and rewrite rules."
  (:require
   [clojure.string :as str]))

;; Tokenizer ------------------------------------------------------------------

(defn code-at
  "The character code at `index` of `text`."
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
  "Whether the character code is a hexadecimal digit."
  [code]
  (or (digit? code) (<= 65 code 70) (<= 97 code 102)))

(defn- space?
  [code]
  (or (= 32 code) (<= 9 code 13)))

(defn- name-char?
  [code]
  (or (letter? code) (digit? code) (= 45 code) (= 95 code)))

(defn- skip-while
  "The first index from `start` whose character `pred?` doesn't hold for."
  [text start pred?]
  (loop [index start]
    (if (and (< index (count text)) (pred? (code-at text index)))
      (recur (inc index))
      index)))

(defn- number-start?
  "Whether a number starts at `index`: digits, or a dot or a sign
  followed by them."
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
  "The number at `index` with its unit, if it has one (`50%`, `90deg`)."
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
  "The token reference at `index`, up to its closing brace. Nil when it
  is never closed."
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
  "The token at `index`, nil when the text can't be one."
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
  "Cuts `text` into tokens, each with the range of the text it is. Nil
  when the text holds something the importers don't read."
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
  "Whether `token` is the punctuation `text`."
  [token text]
  (and (= :punctuation (:type token))
       (= text (:text token))))

(defn parse-value
  "The value at `index`: a call with its arguments, or a single token.
  Returns it with the index after it, nil when the tokens are no value."
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
