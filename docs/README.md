# Tokenscript

Tokenscript is an expression language for design token values:
`{spacing.base} * 2`, `lighten({color.primary}, 0.2)`, `16px`. This
library implements it in Clojure and ClojureScript (`.cljc`), so the
same code resolves tokens on the JVM and in JavaScript. This page maps
the implementation; the pages below describe the language, the schemas
and the resolver.

It is a port of the TypeScript reference implementation
([tokens-studio/tokenscript-interpreter](https://github.com/tokens-studio/tokenscript-interpreter)).

## Pages

- [Language](language.md) — values, operators, units, statements and methods
- [Built-in functions](functions.md) — the functions available without schemas
- [Schemas](schemas.md) — color spaces, functions, units and constants, and the bundled schemas
- [Token validation](validation.md) — token type schemas and their validation scripts
- [Resolver](resolver.md) — resolving token sets, incremental updates and previews
- [Errors and issues](errors.md) — error maps, error codes and the issues of a token
- [Testing](testing.md) — the test tasks and the language tests

## Usage

`tokenscript.core` resolves token sets with the bundled schemas:

```clojure
(require '[tokenscript.core :as ts]
         '[tokenscript.processor :as processor])

(def result
  (ts/build {"base"   {:value "8"}
             "double" {:value "{base} * 2px"}}))

(processor/tokens->strings result)
;; => {"base" "8", "double" "16px"}
```

| Function | Does |
|---|---|
| `default-config` | The config with the bundled schemas, built once |
| `build` | Resolves token data (`{name {:value .. :type ..}}`) or an ordered vector of token sets |
| `color->hex` | Converts any color to a hex color, keeping alpha |

Anything else is used from the implementation namespaces directly.

## Architecture

| Namespace | Role |
|---|---|
| `tokenscript.core` | Facade: resolves token sets with the bundled schemas. |
| `.lexer`, `.parser`, `.ast` | Source text to AST. The lexer is a pure function over a `LexerState` record; the parser is a recursive descent parser. |
| `.interpreter`, `.operations` | Tree walking evaluator. Also runs the scripts of the registered schemas (color initializers and conversions, unit conversions, schema functions). |
| `.symbols` | Runtime values. Scalars are immutable records; lists, dictionaries and tokens keep their content in a volatile because the language mutates them in place (`list.append(x)`). |
| `.functions` | Built-in functions and the registry of schema functions. |
| `.config`, `.registry`, `.colors`, `.units`, `.token-types` | Schema registries. A config is an immutable `Config` record holding one registry record per kind, each implementing `registry/ISchemaRegistry`. |
| `.schema-uri` | Parsing, normalizing and version resolution of schema URIs. |
| `.schemas` | The bundled schemas, read from `resources/tokenscript/schemas`. |
| `.processor`, `.token-data` | The resolver: token sets, dependency resolution, structured tokens, incremental updates. |
| `.validation` | Validation of resolved values against token type schemas. |
| `.errors` | Error codes and error maps. |
| `.util` | JavaScript compatible number formatting and string handling, and case insensitive name tables. |
| `.math` | Host math on doubles, the same results on the JVM and in JS. |
