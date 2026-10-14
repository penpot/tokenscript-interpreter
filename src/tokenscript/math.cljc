;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns tokenscript.math
  "Host math on doubles, the same results on the JVM and in JS."
  (:refer-clojure :exclude [abs]))

(defn nan?
  [v]
  #?(:clj (Double/isNaN v)
     :cljs (js/isNaN v)))

(defn finite?
  "Whether `v` is a finite number. Strings are not numbers, even in JS."
  [v]
  (and (number? v)
       #?(:clj (Double/isFinite v)
          :cljs (js/isFinite v))))

(defn abs
  [v]
  #?(:clj (Math/abs (double v))
     :cljs (js/Math.abs v)))

(defn floor
  [v]
  #?(:clj (Math/floor v)
     :cljs (js/Math.floor v)))

(defn ceil
  [v]
  #?(:clj (Math/ceil v)
     :cljs (js/Math.ceil v)))

(defn sqrt
  [v]
  #?(:clj (Math/sqrt v)
     :cljs (js/Math.sqrt v)))

(defn pow
  [b e]
  #?(:clj (Math/pow b e)
     :cljs (js/Math.pow b e)))

(defn sin
  [v]
  #?(:clj (Math/sin v)
     :cljs (js/Math.sin v)))

(defn cos
  [v]
  #?(:clj (Math/cos v)
     :cljs (js/Math.cos v)))

(defn tan
  [v]
  #?(:clj (Math/tan v)
     :cljs (js/Math.tan v)))

(defn atan2
  [y x]
  #?(:clj (Math/atan2 y x)
     :cljs (js/Math.atan2 y x)))
