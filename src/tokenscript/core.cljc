;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns tokenscript.core
  "Tokenscript: an expression language for design token values.

  This namespace resolves token sets with the schemas shipped with the
  interpreter; the implementation lives in the `tokenscript.*`
  namespaces, which can be used directly for anything else:

    lexer, parser, ast       source text to AST
    interpreter, operations  evaluation
    symbols                  runtime values (numbers, colors, lists, ...)
    config + registries      schemas: color spaces, units, functions,
                             token types (`colors`, `units`, `functions`,
                             `token-types`)
    processor                dependency resolution of token sets and
                             incremental updates
    errors                   structured error maps

  Typical use:

    (build {\"base\" {:value \"8\"}
            \"double\" {:value \"{base} * 2px\"}})
    ;; => {:tokens {...} :issues nil :graph {...} :state ...}"
  (:require
   [tokenscript.config :as config]
   [tokenscript.interpreter :as interp]
   [tokenscript.processor :as processor]
   [tokenscript.schemas :as schemas]))

(def ^:private default-config*
  (delay (config/register-schemas (config/make-config) (schemas/schemas))))

(defn default-config
  "Config with the bundled schemas (color spaces, color functions
  and CSS named colors). Built once."
  []
  @default-config*)

(defn build
  "Resolves token data or an ordered vector of token sets. See
  `tokenscript.processor/build`."
  ([tokens] (build tokens nil))
  ([tokens opts]
   (processor/build tokens (merge {:config (default-config)} opts))))

(defn color->hex
  "Converts any color symbol to its hex representation, keeping alpha."
  ([color] (color->hex (default-config) color))
  ([config color]
   (interp/color->type config color "hex")))
