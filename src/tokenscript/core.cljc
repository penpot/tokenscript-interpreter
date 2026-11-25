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
   [tokenscript.ast :as ast]
   [tokenscript.config :as config]
   [tokenscript.errors :as err]
   [tokenscript.interpreter :as interp]
   [tokenscript.parser :as parser]
   [tokenscript.processor :as processor]
   [tokenscript.registry :as reg]
   [tokenscript.schemas :as schemas]
   [tokenscript.symbols :as sym]
   [tokenscript.units :as units]))

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

(defn literal-number
  "Reads `text` as a literal number: a number (`50`, `.5`, `-4`) or a
  number with a unit (`45deg`, `50%`, `16px`). Gives its number symbol.
  Gives nil for anything else, such as a reference or an expression
  (`{size}`, `{size} * 2`), which only has a value once it resolves."
  ([text] (literal-number (default-config) text))
  ([config text]
   (when-let [ast (and (string? text)
                       (err/language-result #(:ast (parser/parse text {:mode :inline}))))]
     (when (ast/number-node? ast)
       (err/language-result #(interp/interpret ast {:config config}))))))

(defn convert-number
  "Converts `number`, a number with a unit, to the unit `unit`, such as
  `\"deg\"`, with the conversions of the unit registry: `0.25turn` is
  `90deg`. Gives nil when the registry has no conversion between the two
  units, as from `px` to `deg`, or when `number` has no unit."
  ([number unit] (convert-number (default-config) number unit))
  ([config number unit]
   (let [registry (:units config)
         source   (when (sym/number-with-unit? number)
                    (units/uri-by-keyword registry (:unit number)))
         target   (units/uri-by-keyword registry unit)]
     (when (and source target (reg/conversion-steps registry source target))
       (interp/convert-unit config number target)))))
