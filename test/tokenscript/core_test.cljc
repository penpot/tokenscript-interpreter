;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns tokenscript.core-test
  "Tests the helpers of `tokenscript.core` that read the text of a single
  field as tokenscript, without resolving it."
  (:require
   [clojure.test :as t]
   [tokenscript.core :as tsc]
   [tokenscript.symbols :as sym]))

(defn- printed
  "The type and the printed value of a symbol, nil for nil."
  [v]
  (when (some? v)
    [(sym/result-type-name v) (sym/->string-result v (tsc/default-config))]))

(t/deftest literal-number-test
  (t/testing "a number, with or without a unit"
    (t/are [text type value] (= [type value] (printed (tsc/literal-number text)))
      "50"       "Number"               "50"
      ".5"       "Number"               "0.5"
      "-4"       "Number"               "-4"
      " 12.5 "   "Number"               "12.5"
      "45deg"    "NumberWithUnit.Deg"   "45deg"
      "0.25turn" "NumberWithUnit.Turn"  "0.25turn"
      "50%"      "NumberWithUnit.%"     "50%"
      "-4px"     "NumberWithUnit.Px"    "-4px"))

  (t/testing "anything else has no value before it resolves"
    (t/are [text] (nil? (tsc/literal-number text))
      nil
      ""
      "{size}"
      "{size}deg"
      "{size} * 2"
      "1 + 1"
      "-{size}"
      "45 deg"
      "red"
      "#ff0000"
      "rotate(45deg)"
      "1px 2px"))

  (t/testing "with a config of its own, the same reading"
    (t/is (= ["NumberWithUnit.Turn" "0.25turn"]
             (printed (tsc/literal-number (tsc/default-config) "0.25turn"))))
    (t/is (nil? (tsc/literal-number (tsc/default-config) "{size}")))))

(t/deftest convert-number-test
  (t/testing "angles convert to degrees and back"
    (t/are [text unit value] (= value (printed (tsc/convert-number (tsc/literal-number text) unit)))
      "0.25turn" "deg"  ["NumberWithUnit.Deg" "90deg"]
      "100grad"  "deg"  ["NumberWithUnit.Deg" "90deg"]
      "90deg"    "turn" ["NumberWithUnit.Turn" "0.25turn"]
      "90deg"    "grad" ["NumberWithUnit.Grad" "100grad"]
      "45deg"    "deg"  ["NumberWithUnit.Deg" "45deg"]))

  (t/testing "radians go through degrees"
    (t/is (< 57.29 (sym/value (tsc/convert-number (tsc/literal-number "1rad") "deg")) 57.30))
    (t/is (< 1.5707 (sym/value (tsc/convert-number (tsc/literal-number "90deg") "rad")) 1.5708)))

  (t/testing "lengths convert as the registry has them"
    (t/is (= ["NumberWithUnit.Px" "32px"]
             (printed (tsc/convert-number (tsc/literal-number "2rem") "px")))))

  (t/testing "without a conversion there is no result"
    (t/is (nil? (tsc/convert-number (tsc/literal-number "16px") "deg")))
    (t/is (nil? (tsc/convert-number (tsc/literal-number "45deg") "px")))
    (t/is (nil? (tsc/convert-number (tsc/literal-number "45deg") "nope")))
    (t/is (nil? (tsc/convert-number (tsc/literal-number "45") "deg")) "a number without a unit")
    (t/is (nil? (tsc/convert-number nil "deg")))))
