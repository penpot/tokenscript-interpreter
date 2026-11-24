;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns tokenscript.importers.css-test
  (:require
   [clojure.test :as t]
   [tokenscript.importers.css :as css]))

(defn- stops
  [text]
  (get (css/linear-gradient text) "stops"))

(t/deftest linear-gradient-test
  (t/testing "a side as the direction, the stops spread from start to end"
    (t/is (= {"kind" "linear"
              "angle" "90"
              "stops" [{"color" "#5500ff" "position" "0%"}
                       {"color" "#00ff55" "position" "100%"}]}
             (css/linear-gradient "linear-gradient(to right, #5500ff, #00ff55)"))))

  (t/testing "without a direction it goes from top to bottom"
    (t/is (= "180" (get (css/linear-gradient "linear-gradient(red, blue)") "angle"))))

  (t/testing "the direction is a side, a corner or an angle in any unit"
    (t/are [direction angle] (= angle
                                (get (css/linear-gradient
                                      (str "linear-gradient(" direction ", red, blue)"))
                                     "angle"))
      "to top"          "0"
      "to left"         "270"
      "to bottom right" "135"
      "to left top"     "315"
      "TO RIGHT"        "90"
      "45deg"           "45"
      "-90deg"          "-90"
      "12.5deg"         "12.5"
      "0.25turn"        "90"
      "200grad"         "180"
      "3.1416rad"       "180"))

  (t/testing "stops keep their positions, the others are spread between them"
    (t/is (= ["0%" "10%" "40%" "70%" "100%"]
             (map #(get % "position")
                  (stops "linear-gradient(red, orange 10%, yellow, green, blue)")))))

  (t/testing "a stop at 0 is at 0%"
    (t/is (= ["0%" "100%"]
             (map #(get % "position") (stops "linear-gradient(red 0, blue)")))))

  (t/testing "colors are kept as written"
    (t/is (= ["red" "#00f" "#ff000080" "hsl(120, 100%, 50%)" "oklch(0.6 0.2 30)"]
             (map #(get % "color")
                  (stops "linear-gradient(red, #00f, #ff000080, hsl(120, 100%, 50%), oklch(0.6 0.2 30))")))))

  (t/testing "transparent is black with no alpha, in any case"
    (t/is (= ["#00000000" "black" "#00000000"]
             (map #(get % "color")
                  (stops "linear-gradient(transparent, black, Transparent)")))))

  (t/testing "CSS color functions get commas, and the slash before the alpha goes"
    (t/is (= ["rgba(255, 0, 0, 50%)" "rgba(255, 209, 102, 0.6)" "hsla(120deg, 100%, 50%, 0.5)" "rgba(1, 2, 3, .4)"
              "rgba(255, 0, 0)" "hsl(1, 0.5, 0.5)"]
             (map #(get % "color")
                  (stops (str "linear-gradient(rgb(255 0 0 / 50%), rgba(255, 209, 102, 0.6), "
                              "hsl(120deg 100% 50% / 0.5), rgba(1 2 3 / .4), rgba(255 0 0), hsl(1 0.5 0.5))"))))))

  (t/testing "token references stay, as colors, positions and inside colors"
    (t/is (= [{"color" "{color.brand}" "position" "{position.start}"}
              {"color" "rgba({color.accent}, 0.5)" "position" "100%"}]
             (stops "linear-gradient(90deg, {color.brand} {position.start}, rgba({color.accent}, 0.5))"))))

  (t/testing "a color with two positions is two stops"
    (t/is (= [["red" "0%"] ["red" "50%"] ["blue" "50%"] ["blue" "100%"]]
             (map (juxt #(get % "color") #(get % "position"))
                  (stops "linear-gradient(red 0% 50%, blue 50% 100%)")))))

  (t/testing "a color hint is left out"
    (t/is (= ["red" "blue"]
             (map #(get % "color") (stops "linear-gradient(red, 30%, blue)")))))

  (t/testing "positions outside the gradient move to its ends"
    (t/is (= ["0%" "100%"]
             (map #(get % "position") (stops "linear-gradient(red -20%, blue 150%)")))))

  (t/testing "it can be given over several lines, as a declaration"
    (t/is (= (css/linear-gradient "linear-gradient(red, blue)")
             (css/linear-gradient " LINEAR-GRADIENT( red,blue ) ")
             (css/linear-gradient "background: linear-gradient(red, blue);")
             (css/linear-gradient "background-image:\nlinear-gradient(\n  red,\n  blue\n);"))))

  (t/testing "the declarations a CSS tool copies: the gradient is the one that counts"
    (t/is (= (css/linear-gradient "linear-gradient(90deg, rgba(42, 123, 155, 1) 0%, rgba(87, 199, 133, 1) 50%, rgba(237, 221, 83, 1) 100%)")
             (css/linear-gradient (str "background: #2A7B9B;background: linear-gradient(90deg,"
                                       "rgba(42, 123, 155, 1) 0%, rgba(87, 199, 133, 1) 50%, rgba(237, 221, 83, 1) 100%);")))
          "a fallback color first")
    (t/is (= ["90" ["#020024" "#00d4ff"]]
             (let [gradient (css/linear-gradient
                             (str "/* generated */\n"
                                  "background: #020024;\n"
                                  "background: -webkit-linear-gradient(left, #111111 0%, #222222 100%);\n"
                                  "background: linear-gradient(90deg, #020024 0%, #00d4ff 100%);\n"
                                  "filter: progid:DXImageTransform.Microsoft.gradient(startColorstr='#020024', endColorstr='#00d4ff', GradientType=1);"))]
               [(get gradient "angle") (map #(get % "color") (get gradient "stops"))]))
          "vendor lines, comments and an IE filter")
    (t/is (nil? (css/linear-gradient "background: #2A7B9B; color: red;")))
    (t/is (nil? (css/linear-gradient "/* never closed linear-gradient(red, blue)")))
    (t/is (nil? (css/linear-gradient "background: url('unclosed;"))))

  (t/testing "anything else is no linear gradient"
    (t/are [text] (nil? (css/linear-gradient text))
      nil
      ""
      "#ff0000"
      "{gradient.brand}"
      "linear-gradient"
      "linear-gradient()"
      "linear-gradient(red)"
      "linear-gradient(to right, red)"
      "linear-gradient(red, blue"
      "linear-gradient(red, blue))"
      "linear-gradient(red,, blue)"
      "linear-gradient(to nowhere, red, blue)"
      "linear-gradient(red 10px, blue 20px)"
      "linear-gradient(red 10% 20% 30%, blue)"
      "linear-gradient(#12345, blue)"
      "linear-gradient(currentcolor, blue)"
      "linear-gradient(red, currentColor)"
      "linear-gradient({color.brand, blue)"
      "linear-gradient(red, blue), linear-gradient(green, yellow)"
      "linear-gradient(red, blue) no-repeat"
      "radial-gradient(red, blue)"
      "conic-gradient(red, blue)"
      "repeating-linear-gradient(red, blue)")))

(t/deftest linear-gradient->tokenscript-test
  (t/testing "CSS is written as the tokenscript call"
    (t/is (= "linear-gradient(90deg, #5500ff 0%, rgba(255, 209, 102, 0.6) 35%, #00ff55 100%)"
             (css/linear-gradient->tokenscript
              "linear-gradient(to right, #5500ff, rgb(255 209 102 / 0.6) 35%, #00ff55)"))))

  (t/testing "references are written as they are"
    (t/is (= "linear-gradient(-45deg, {color.brand} {position.start}, #0000ff 100%)"
             (css/linear-gradient->tokenscript
              "linear-gradient(-45deg, {color.brand} {position.start}, #0000ff)"))))

  (t/testing "what is written reads back the same"
    (let [tokenscript (css/linear-gradient->tokenscript "linear-gradient(0.5turn, red, rgb(0 0 255 / 50%) 40%, blue)")]
      (t/is (= (css/linear-gradient tokenscript)
               (css/linear-gradient (css/linear-gradient->tokenscript tokenscript))))))

  (t/testing "anything else is not converted"
    (t/is (nil? (css/linear-gradient->tokenscript "radial-gradient(red, blue)")))
    (t/is (nil? (css/linear-gradient->tokenscript "{gradient.base}")))))
