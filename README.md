# tokenscript-interpreter

Interpreter of the Tokenscript language for design tokens, in Clojure
and ClojureScript.

Tokenscript is an expression language for design token values:
`{spacing.base} * 2`, `lighten({color.primary}, 0.2)`, `16px`. This
library parses and evaluates it, and resolves whole token sets whose
values reference each other, with incremental updates. The same code
runs on the JVM and in JavaScript. Penpot uses it to resolve design
tokens.

It is a port of the TypeScript reference implementation,
[tokens-studio/tokenscript-interpreter](https://github.com/tokens-studio/tokenscript-interpreter).

## Installation

Add it as a git dependency to `deps.edn`:

```clojure
{:deps
 {penpot/tokenscript-interpreter
  {:git/url "https://github.com/penpot/tokenscript-interpreter"
   :git/sha "<commit sha>"}}}
```

ClojureScript builds need shadow-cljs: the bundled schemas are inlined at
compile time by a macro that records the files it reads.

## Usage

```clojure
(require '[tokenscript.core :as ts]
         '[tokenscript.processor :as processor])

(def result
  (ts/build {"base"   {:value "8"}
             "double" {:value "{base} * 2px"}}))

(processor/tokens->strings result)
;; => {"base" "8", "double" "16px"}
```

## Documentation

See [docs/](docs/README.md):

- [Language](docs/language.md)
- [Built-in functions](docs/functions.md)
- [Schemas](docs/schemas.md)
- [Token validation](docs/validation.md)
- [Resolver](docs/resolver.md)
- [Errors and issues](docs/errors.md)
- [Testing](docs/testing.md)

## Development

Needs the Clojure CLI, [Babashka](https://babashka.org) and Node.js.

```bash
bb test   # JVM and ClojureScript tests
bb lint   # clj-kondo and cljfmt check
```

See [Testing](docs/testing.md) for the other tasks and
[CONTRIBUTING.md](CONTRIBUTING.md) for the commit format.

## License

[MPL-2.0](LICENSE).
