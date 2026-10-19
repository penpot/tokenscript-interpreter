;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns tokenscript.language-test
  "The language end to end: literals and references, strings and
  implicit lists, math, control flow, the List and Dictionary methods,
  `type()` and null handling. A result is the printed value with its
  full type name, or the code of the raised error."
  (:require
   [clojure.test :as t]
   [tokenscript.errors :as err]
   [tokenscript.symbols :as sym]
   [tokenscript.test-helpers :as h]))

(defn- run
  [text references mode]
  (try
    (let [result (h/evaluate text references {:mode mode})]
      [(sym/->string-result result (h/core-config)) (sym/result-type-name result)])
    (catch #?(:clj Throwable :cljs :default) e
      (if (err/language-error? e)
        (:code (ex-data e))
        (throw e)))))

(defn- inline
  "Result of a token value expression."
  ([text] (inline text nil))
  ([text references] (run text references :inline)))

(defn- script
  "Result of a script."
  ([text] (script text nil))
  ([text references] (run text references :script)))

(t/deftest references-and-literals-test
  (t/is (= ["2" "Number"] (script "{🔝}" {"🔝" 2})))
  (t/is (= ["🌈" "String"] (inline "{tokenscript}" {"tokenscript" "🌈"})))
  (t/is (= ["1" "Number"] (script "variable 🍑: Number = 1; return 🍑;")))
  (t/is (= ["😍" "String"] (script "variable tokenscript: String = \"😍\"; return tokenscript;")))
  (t/is (= ["HELLO, WORLD" "List"] (inline "\"HELLO WORLD\".spLit(\" \")")))
  (t/is (= ["hello" "String"] (inline "\"HELLO\".loweR()")))
  (t/is (= ["#ff0000" "Color.Hex"] (script "variable c: Color = #ff0000; return c;")))
  (t/is (= ["175px" "NumberWithUnit.Px"] (inline "({hello}rem + 9rem) / 2 + 55px" {"hello" 6})))
  (t/is (= ["6rem" "NumberWithUnit.Rem"] (inline "{x} * {x} + {x}rem" {"x" 2})))
  (t/is (= ["2rem" "NumberWithUnit.Rem"] (inline "{x}rem" {"x" 2})))
  (t/is (= ["6rem" "NumberWithUnit.Rem"] (inline "{x} * {x} + {x}" {"x" (sym/number-with-unit 2 "rem")})))
  (t/is (= ["true" "Boolean"] (script "variable a: Boolean = true; return a;")))
  (t/is (= ["6" "Number"] (inline "{hello }" {"hello" 6})))
  (t/is (= :operations/cannot-compare-units (inline "12rem == 12px")))
  (t/is (= ["8rem" "NumberWithUnit.Rem"] (inline "{a} * ({a} + {a})rem" {"a" 2})))
  (t/is (= :lexer/invalid-character (inline "{a} * {a} + {a} ´ 2" {"a" 2})))
  (t/is (= ["10" "Number"] (script "variable n: Number = 0; while(n < 10) [ n = n+1; ] return n;"))))

(t/deftest strings-and-implicit-lists-test
  (t/is (= ["1unknown" "String"] (inline "\"1unknown\"")))
  (t/is (= ["3D Font" "String"] (inline "\"3D Font\"")))
  (t/is (= ["1 + 1" "String"] (inline "\"1 + 1\"")))
  (t/is (= ["10 20 30" "List.Implicit"] (inline "10 20 30")))
  (t/is (= ["10px 20px 30px 40px" "List.Implicit"] (inline "10px 20px 30px 40px")))
  (t/is (= ["3 D Font" "List.Implicit"] (inline "3D Font")))
  (t/is (= ["test123" "String"] (inline "test123")))
  (t/is (= ["2 unknown" "List.Implicit"] (inline "1 + 1 unknown")))
  (t/is (= ["9 foo" "List.Implicit"] (inline "(1 + 2) * 3foo")))
  (t/is (= ["1.5 unknown" "List.Implicit"] (inline "1.5unknown")))
  (t/is (= ["-1 unknown" "List.Implicit"] (inline "-1unknown")))
  (t/is (= ["5 test" "List.Implicit"] (inline "5test")))
  (t/is (= ["my_variable" "String"] (inline "my_variable")))
  (t/is (= ["hello world" "String"] (inline "\"HELLO\".lower().concat(\" WORLD\".lower())")))
  (t/is (= ["4" "Number"] (inline "\"hello\".length() - 1")))
  (t/is (= ["hello" "String"] (inline "\"HELLO\".lower()")))
  (t/is (= ["H, E, L, L, O" "List"] (inline "\"HELLO\".split()")))
  (t/is (= ["HELLO, WORLD" "List"] (inline "\"HELLO🔥WORLD\".split(\"🔥\")")))
  (t/is (= ["HELLO, WORLD" "List"] (inline "\"HELLO WORLD\".split(\" \")"))))

(t/deftest inline-math-test
  (t/is (= ["7" "Number"] (inline "4+{x}" {"x" 3})))
  (t/is (= ["2.333333333333333" "Number"] (inline "(1 + -3) * 2 + (4/3) + 5")))
  ;; JavaScript number formatting: 7, not 7.0
  (t/is (= ["7" "Number"] (inline "4 + 2.5 + 3 - 2.5")))
  (t/is (= ["9" "Number"] (inline "4 + 2 * 3 - 5 / 5")))
  (t/is (= ["6" "Number"] (inline "4 + -2 + 3 - -1")))
  (t/is (= ["1rem" "NumberWithUnit.Rem"] (inline "10rem * 10%"))))

(t/deftest math-functions-test
  (t/is (= ["99999999999999" "Number"] (inline "abs(-99999999999999)")))
  (t/is (= ["2.5" "Number"] (inline "average(3,2,3,2)")))
  (t/is (= ["4" "Number"] (inline "ceil(3.1)")))
  (t/is (= ["3" "Number"] (inline "floor(3.1)")))
  (t/is (= ["3" "Number"] (inline "max(3,2,1)")))
  (t/is (= ["1" "Number"] (inline "min(3,2,1)")))
  (t/is (= ["1" "Number"] (inline "mod(3,2)")))
  (t/is (= ["3.14159" "Number"] (inline "round_to(pi(), 5)")))
  (t/is (= ["8" "Number"] (inline "pow(2, 3)")))
  (t/is (= ["9.33" "Number"] (inline "round_to(9.33339, 2)")))
  (t/is (= ["4" "Number"] (inline "round(3.5)")))
  (t/is (= ["3" "Number"] (inline "round(2.51)")))
  (t/is (= ["3" "Number"] (inline "sqrt(9)")))
  (t/is (= ["10" "Number"] (inline "sum(3,2,2,3)"))))

(t/deftest control-flow-test
  (t/is (= ["10" "Number"] (script "variable x: Number = 0;
variable y: Number = 5;
if(x < y && true) [
    x = 10;
];
return x;")))
  (t/is (= ["true" "Boolean"] (script "variable x: Number = 10;
 if(x == 5) [
   return false;
] elif (x==10) [ return true;] else [return false;];
return false;")))
  (t/is (= :parser/condition-must-be-boolean (script "variable x: Number = 10;
 if(x == 5) [
   return false;
] elif (x=10) [ return true;] else [return false;];
return false;")))
  (t/is (= ["true" "Boolean"] (script "variable not_result: boolean = {a} != {b} && !{b} ; return not_result;" {"a" true "b" false})))
  (t/is (= ["true" "Boolean"] (script "variable a_boolean: Boolean = true;
variable another_boolean: Boolean = false;

if(a_boolean == true) [
    return true;
]

return false;")))
  (t/is (= ["10" "Number"] (script "variable x: Number = 0; if(false) [ x = 5; ] else [ x = 10; ]; return x;")))
  (t/is (= :interpreter/max-iterations-exceeded (script "variable j: Number = 0; while(true) [ j = j + 1; ]; return j;")))
  (t/is (= :interpreter/while-condition-not-boolean (script "variable i: Number = 0; while(5) [ i = i + 1; ]; return i;")))
  (t/is (= ["3px" "NumberWithUnit.Px"] (script "variable i: NumberWithUnit = 0px;
variable j: Number = 0;
while(i < 3) [
    j = 0;
    while(j < 2) [
        j = j + 1;
    ];
    i = i + 1;
];
return i;"))))

(t/deftest list-methods-test
  (t/is (= ["1, 2, 3, 4" "List"] (script "variable x: List = 1, 2; x.append(3).append(4); return x;")))
  (t/is (= :symbols/index-out-of-range (script "variable x: List = 1, 2, 3; x.delete(5); return x;")))
  (t/is (= ["2" "Number"] (script "variable x: List = 1, 2, 3, 4; variable idx: Number = x.index(3); return idx;")))
  (t/is (= ["1, 2, 3" "List"] (script "variable x: List = 1, 2; x.append(3); return x")))
  (t/is (= ["1, 2, 3" "List"] (script "variable x: List = 1, 2, 3; return x;")))
  (t/is (= ["1, 2, 4" "List"] (script "variable x: List = 1, 2, 3, 4; x.delete(2); return x;")))
  (t/is (= ["1, 2, 3, 4" "List"] (script "variable x: List = 1, 2; variable y: List = 3, 4; x.extend(y); return x;")))
  (t/is (= ["-1" "Number"] (script "variable x: List = 1, 2, 3, 4; variable idx: Number = x.index(5); return idx;")))
  (t/is (= ["1, 2, 3, 4" "List"] (script "variable x: List = 1, 2, 4; x.insert(2, 3); return x;")))
  (t/is (= :symbols/index-out-of-range (script "variable x: List = 1, 2, 3; x.insert(5, 4); return x;")))
  (t/is (= ["4" "Number"] (script "variable x: List = 1, 2, 3, 4; variable len: Number = x.length(); return len;")))
  ;; list strings print without quotes
  (t/is (= ["1, hello, true" "List"] (script "variable x: List = 1, \"hello\", true; return x;"))))

(t/deftest dictionary-methods-test
  (t/is (= ["1" "Number"] (script "variable x: Dictionary;
   x.set(a, 1);
   x.set(b, 2); x.set(dict, x); 
return x.get(dict).get(a);")))
  (t/is (= ["0" "Number"] (script "variable x: Dictionary; x.set(a, 1); x.set(b, 2); x.clear(); return x.length();")))
  (t/is (= ["1" "Number"] (script "variable x: Dictionary;
   x.set(a, 1);
   x.set(b, 2);  x.delete(b); 
return x.length();")))
  (t/is (= ["1" "Number"] (script "variable a: Number = 1;
variable x: Dictionary;
x.set('a', a);
a = 2;
return x.get('a');")))
  (t/is (= ["1" "Number"] (script "variable x: Dictionary; x.set(\"a\", 1); x.set(\"b\", 2); return x.get(a);")))
  (t/is (= ["true" "Boolean"] (script "variable x: Dictionary; x.set(a, 1); x.set(b, 2); return x.key_exists(a);")))
  (t/is (= ["false" "Boolean"] (script "variable x: Dictionary; x.set(a, 1); x.set(b, 2); return x.key_exists(z);")))
  (t/is (= ["a, b" "List"] (script "variable x: Dictionary; x.set(a, 1); x.set(b, 2); return x.keys();"))))

(t/deftest color-scripts-test
  (t/is (= ["255, 87, 51" "List"] (script "variable color_parts: List = {COLOR}.to_string().split(\"#\");
variable color: List = color_parts.get(1).split();
variable length: Number = color.length();
variable rgb: List = 0, 0, 0;
if(length == 3) [
    rgb.update(0, parse_int(color.get(0).concat(color.get(0)), 16));
    rgb.update(1, parse_int(color.get(1).concat(color.get(1)), 16));
    rgb.update(2, parse_int(color.get(2).concat(color.get(2)), 16));
] else [
    rgb.update(0, parse_int(color.get(0).concat(color.get(1)), 16));
    rgb.update(1, parse_int(color.get(2).concat(color.get(3)), 16));
    rgb.update(2, parse_int(color.get(4).concat(color.get(5)), 16));
];

return rgb;" {"COLOR" "#FF5733"})))
  (t/is (= ["1, 0, 0" "List"] (script "variable rgb: List = {r}, {g}, {b};
variable rgb_linear: List = 0, 0, 0;
variable gamma: Number = 2.4;

// Convert RGB to linear RGB
variable r: Number = rgb.get(0) / 255;
variable g: Number = rgb.get(1) / 255;
variable b: Number = rgb.get(2) / 255;

// Process red channel
if(r <= 0.03928) [
    rgb_linear.update(0, r / 12.92);
] else [
    rgb_linear.update(0, pow((r + 0.055) / 1.055, gamma));
];

// Process green channel
if(g <= 0.03928) [
    rgb_linear.update(1, g / 12.92);
] else [
    rgb_linear.update(1, pow((g + 0.055) / 1.055, gamma));
];

// Process blue channel
if(b <= 0.03928) [
    rgb_linear.update(2, b / 12.92);
] else [
    rgb_linear.update(2, pow((b + 0.055) / 1.055, gamma));
];

return rgb_linear;" {"r" 255 "g" 0 "b" 0}))))

(t/deftest type-function-test
  (t/is (= ["boolean" "String"] (script "return type(true)")))
  ;; type() returns the qualified type
  (t/is (= ["color.hex" "String"] (script "variable a_color: Color = #333;return type(a_color)")))
  (t/is (= ["number" "String"] (script "return type(2.4);")))
  (t/is (= ["string" "String"] (script "return type(\"abc\")")))
  (t/is (= ["string" "String"] (script "variable a_string: String = abc;return type(a_string)"))))

(t/deftest null-values-test
  (t/is (= ["null" "Boolean"] (script "variable x: Boolean; return x;")))
  ;; a null unit prints as nullpx, a null color as an empty string
  (t/is (= ["nullpx" "NumberWithUnit.Px"] (script "variable x: NumberWithUnit; return x;")))
  ;; a null unit prints as nullpx, a null color as an empty string
  (t/is (= ["" "Color"] (script "variable x: Color; return x;")))
  ;; a null unit prints as nullpx, a null color as an empty string
  (t/is (= ["" "Color.Hex"] (script "variable x: Color.Hex; return x;")))
  (t/is (= ["null" "Null"] (script "variable dict: Dictionary; return dict.get(\"nonexistent\");")))
  (t/is (= ["null" "String"] (script "variable dict: Dictionary; variable str: String = dict.get(\"foo\"); return str;")))
  (t/is (= ["null" "Number"] (script "variable dict: Dictionary; variable num: Number = dict.get(\"foo\"); return num;")))
  (t/is (= ["null" "Boolean"] (script "variable dict: Dictionary; variable bool: Boolean = dict.get(\"foo\"); return bool;")))
  ;; a null unit prints as nullpx, a null color as an empty string
  (t/is (= ["nullpx" "NumberWithUnit.Px"] (script "variable dict: Dictionary; variable nwu: NumberWithUnit = dict.get(\"foo\"); return nwu;")))
  ;; a null unit prints as nullpx, a null color as an empty string
  (t/is (= ["" "Color"] (script "variable dict: Dictionary; variable color: Color = dict.get(\"foo\"); return color;")))
  ;; a null unit prints as nullpx, a null color as an empty string
  (t/is (= ["" "Color.Hex"] (script "variable dict: Dictionary; variable hex: Color.Hex = dict.get(\"foo\"); return hex;")))
  ;; an empty list prints as an empty string
  (t/is (= ["" "List"] (script "variable list: List; return list;")))
  (t/is (= ["{}" "Dictionary"] (script "variable dict: Dictionary; return dict;")))
  (t/is (= ["null" "String"] (script "variable a: String; return a;")))
  (t/is (= ["null" "Boolean"] (script "variable c: Dictionary;
variable foo: Boolean = c.get(\"nonexistent\");
return foo;")))
  ;; a null unit prints as nullpx, a null color as an empty string
  (t/is (= ["" "Color"] (script "variable c: Dictionary; variable foo: Color = c.get(\"nonexistent\"); return foo;")))
  (t/is (= ["null" "Number"] (script "variable c: Dictionary; variable foo: Number = c.get(\"nonexistent\"); return foo;")))
  ;; a null unit prints as nullpx, a null color as an empty string
  (t/is (= ["nullpx" "NumberWithUnit.Px"] (script "variable c: Dictionary;
variable foo: NumberWithUnit = c.get(\"nonexistent\");
return foo;")))
  (t/is (= ["null" "String"] (script "variable c: Dictionary; variable foo: String = c.get(\"nonexistent\"); return foo;")))
  (t/is (= ["null" "Null"] (script "return null;"))))

(t/deftest null-equality-test
  (t/is (= ["false" "Boolean"] (script "variable a: String = \"hello\"; return a == null;")))
  (t/is (= ["true" "Boolean"] (script "variable a: String = \"hello\"; return a != null;")))
  (t/is (= ["false" "Boolean"] (inline "null != null")))
  (t/is (= ["true" "Boolean"] (script "variable x: String; return x == null;")))
  (t/is (= ["true" "Boolean"] (script "variable x: Boolean; return x == null;")))
  (t/is (= ["true" "Boolean"] (script "variable x: NumberWithUnit; return x == null;")))
  (t/is (= ["true" "Boolean"] (script "variable x: Color; return x == null;")))
  (t/is (= ["true" "Boolean"] (script "variable x: Color.Hex; return x == null;")))
  (t/is (= ["false" "Boolean"] (script "variable x: String = \"hello\"; return x == null;")))
  (t/is (= ["false" "Boolean"] (script "variable x: Number = 42; return x == null;")))
  (t/is (= ["false" "Boolean"] (script "variable x: Boolean = true; return x == null;")))
  (t/is (= ["false" "Boolean"] (script "variable x: NumberWithUnit = 10px; return x == null;")))
  (t/is (= ["false" "Boolean"] (script "variable x: Color = #ff0000; return x == null;")))
  (t/is (= ["false" "Boolean"] (script "variable x: List; return x == null;")))
  (t/is (= ["false" "Boolean"] (script "variable x: List = 1, 2, 3; return x == null;")))
  (t/is (= ["false" "Boolean"] (script "variable x: Dictionary; return x == null;")))
  (t/is (= ["false" "Boolean"] (script "variable dict: Dictionary; dict.set(\"key\", \"value\"); return dict == null;")))
  (t/is (= ["false" "Boolean"] (script "variable x: String; return x != null;")))
  (t/is (= ["true" "Boolean"] (script "variable x: String = \"hello\"; return x != null;")))
  (t/is (= ["true" "Boolean"] (script "variable x: List; return x != null;")))
  (t/is (= ["true" "Boolean"] (script "variable x: Dictionary; return x != null;")))
  (t/is (= ["true" "Boolean"] (script "variable dict: Dictionary; variable value: String = dict.get(\"nonexistent\"); return value == null;")))
  (t/is (= ["true" "Boolean"] (script "variable a: String; variable b: Number; variable c: List; return (a == null) && (b == null) && (c != null);")))
  (t/is (= ["is null" "String"] (script "variable a: String; if (a == null) [ return \"is null\"; ] else [ return \"not null\"; ];")))
  (t/is (= ["not null" "String"] (script "variable a: String = \"hello\"; if (a == null) [ return \"is null\"; ] else [ return \"not null\"; ];")))
  (t/is (= ["true" "Boolean"] (script "variable a: Number; return a == null;")))
  (t/is (= ["true" "Boolean"] (script "variable a: String; return a == null;")))
  (t/is (= ["false" "Boolean"] (script "variable a: String; return a != null;")))
  (t/is (= ["true" "Boolean"] (script "variable dict: Dictionary; variable nullStr: String = dict.get(\"foo\"); variable emptyStr: String; return nullStr == emptyStr;")))
  (t/is (= ["true" "Boolean"] (script "variable dict: Dictionary; variable nullNum: Number = dict.get(\"foo\"); variable emptyNum: Number; return nullNum == emptyNum;")))
  (t/is (= ["true" "Boolean"] (script "variable dict: Dictionary; variable nullBool: Boolean = dict.get(\"foo\"); variable emptyBool: Boolean; return nullBool == emptyBool;")))
  (t/is (= ["true" "Boolean"] (script "variable dict: Dictionary; variable nullUnit: NumberWithUnit = dict.get(\"foo\"); variable emptyUnit: NumberWithUnit; return nullUnit == emptyUnit;")))
  (t/is (= ["true" "Boolean"] (script "variable dict: Dictionary; variable nullColor: Color = dict.get(\"foo\"); variable emptyColor: Color; return nullColor == emptyColor;")))
  (t/is (= ["true" "Boolean"] (script "variable dict: Dictionary; variable nullHex: Color.Hex = dict.get(\"foo\"); variable emptyHex: Color.Hex; return nullHex == emptyHex;")))
  (t/is (= ["false" "Boolean"] (script "variable dict: Dictionary; variable nullStr: String = dict.get(\"foo\"); variable emptyStr: String; return nullStr != emptyStr;")))
  (t/is (= ["false" "Boolean"] (script "variable dict: Dictionary; variable nullStr: String = dict.get(\"foo\"); variable valueStr: String = \"hello\"; return nullStr == valueStr;")))
  (t/is (= ["false" "Boolean"] (script "variable dict: Dictionary; variable nullNum: Number = dict.get(\"foo\"); variable valueNum: Number = 42; return nullNum == valueNum;")))
  (t/is (= ["false" "Boolean"] (script "variable dict: Dictionary; variable nullBool: Boolean = dict.get(\"foo\"); variable valueBool: Boolean = true; return nullBool == valueBool;")))
  (t/is (= ["false" "Boolean"] (script "variable dict: Dictionary; variable nullUnit: NumberWithUnit = dict.get(\"foo\"); variable valueUnit: NumberWithUnit = 10px; return nullUnit == valueUnit;")))
  (t/is (= ["false" "Boolean"] (script "variable dict: Dictionary; variable nullColor: Color = dict.get(\"foo\"); variable valueColor: Color = #ff0000; return nullColor == valueColor;")))
  (t/is (= ["true" "Boolean"] (script "variable dict: Dictionary; variable nullStr1: String = dict.get(\"foo\"); variable nullStr2: String = dict.get(\"bar\"); return nullStr1 == nullStr2;")))
  (t/is (= ["true" "Boolean"] (script "variable dict: Dictionary; variable nullStr: String = dict.get(\"foo\"); variable emptyStr: String; variable valueStr: String = \"test\"; return (nullStr == emptyStr) && (nullStr != valueStr);")))
  (t/is (= ["both null" "String"] (script "variable dict: Dictionary; variable nullStr: String = dict.get(\"foo\"); variable emptyStr: String; if (nullStr == emptyStr) [ return \"both null\"; ] else [ return \"not equal\"; ];")))
  (t/is (= ["not equal" "String"] (script "variable dict: Dictionary; variable nullStr: String = dict.get(\"foo\"); variable valueStr: String = \"hello\"; if (nullStr == valueStr) [ return \"equal\"; ] else [ return \"not equal\"; ];")))
  (t/is (= ["true" "Boolean"] (script "variable dict: Dictionary; variable nullStr: String = dict.get(\"foo\"); variable nullNum: Number = dict.get(\"bar\"); return nullStr == null && nullNum == null;")))
  (t/is (= ["true" "Boolean"] (script "variable dict1: Dictionary; variable dict2: Dictionary; variable null1: String = dict1.get(\"missing1\"); variable null2: String = dict2.get(\"missing2\"); return null1 == null2;")))
  (t/is (= ["true" "Boolean"] (script "variable dict: Dictionary; variable result: String = dict.get(\"nonexistent1\"); variable result2: String = dict.get(\"nonexistent2\"); return result == result2 && result == null;")))
  (t/is (= ["true" "Boolean"] (script "variable dict: Dictionary;
variable retrieved: Color = dict.get(\"nonexistent\");
return retrieved == null;")))
  (t/is (= ["true" "Boolean"] (script "variable dict: Dictionary;
variable retrieved: Number = dict.get(\"nonexistent\");
return retrieved == null;")))
  (t/is (= ["true" "Boolean"] (script "variable dict: Dictionary;
variable retrieved: String = dict.get(\"nonexistent\");
return retrieved == null;"))))

(t/deftest null-ordering-test
  (t/is (= :operations/ordering-with-null (inline "null > 5")))
  (t/is (= :operations/ordering-with-null (inline "null < 5")))
  (t/is (= :operations/ordering-with-null (inline "null >= 5")))
  (t/is (= :operations/ordering-with-null (inline "null <= 5")))
  (t/is (= :operations/ordering-with-null (inline "5 > null")))
  (t/is (= :operations/ordering-with-null (inline "5 < null")))
  (t/is (= :operations/ordering-with-null (inline "5 >= null")))
  (t/is (= :operations/ordering-with-null (inline "5 <= null")))
  (t/is (= :operations/ordering-with-null (inline "null > null")))
  (t/is (= :operations/ordering-with-null (inline "null < null")))
  (t/is (= :operations/ordering-with-null (inline "null >= null")))
  (t/is (= :operations/ordering-with-null (inline "null <= null")))
  (t/is (= :operations/ordering-with-null (script "variable a: Number; return a > 42;")))
  (t/is (= :operations/ordering-with-null (script "variable a: Number; return a < 42;")))
  (t/is (= :operations/ordering-with-null (script "variable a: NumberWithUnit; return a > 10px;")))
  (t/is (= :operations/ordering-with-null (script "variable a: NumberWithUnit; return a < 10px;")))
  (t/is (= :operations/ordering-with-null (script "variable a: Number; variable b: Number; return a < b;")))
  (t/is (= :operations/ordering-with-null (script "variable dict: Dictionary; variable nullValue: String = dict.get(\"nonexistent\"); return nullValue > \"test\";")))
  (t/is (= :operations/ordering-with-null (script "variable a: Number; if (a > 0) [ return \"positive\"; ] else [ return \"not positive\"; ];")))
  (t/is (= :operations/ordering-with-null (script "return null > null;")))
  (t/is (= :operations/ordering-with-null (script "variable a: String = \"test\"; return a > null;")))
  (t/is (= :operations/ordering-with-null (script "variable a: String; return a > \"test\";")))
  (t/is (= :operations/ordering-with-null (script "variable a: Boolean; return a >= true;")))
  (t/is (= :operations/ordering-with-null (script "variable a: Number; return a < 42;")))
  (t/is (= :operations/ordering-with-null (script "variable a: NumberWithUnit; return a <= 10px;"))))
