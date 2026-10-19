# Testing tokenscript

The tests are in `test/tokenscript/` and run on the JVM and in
ClojureScript (node), through the tasks in `bb.edn`:

```bash
bb test                       # both
bb test:jvm                   # JVM, all namespaces
bb test:jvm lexer processor   # JVM, tokenscript.lexer-test and tokenscript.processor-test
bb test:cljs                  # ClojureScript on node
bb lint                       # clj-kondo and cljfmt check
bb fmt                        # cljfmt fix
```

CI runs `bb lint` and the tests on both platforms for every pull
request.

## Language tests

`tokenscript.language-test` covers the language end to end: literals
and references, strings and implicit lists, math, control flow, the
List and Dictionary methods, `type()` and null handling. Each case
evaluates a token value (`inline`) or a script (`script`) and checks
the printed value with its full type name, or the code of the raised
error. Tests assert error codes, never messages: the interpreter has
none (see [Errors and issues](errors.md)).

Some outputs follow the TypeScript reference interpreter, and the tests
pin them:

- `4 + 2.5 + 3 - 2.5` prints as `7`, not `7.0`.
- `round(2.5)` is `3` (JavaScript rounding).
- Strings inside lists print without quotes.
- `type(#333)` is `color.hex`.
- Null colors print as an empty string and null units as `nullpx`.
- Empty lists print as an empty string.

## History: why not the compliance suite

The interpreter was ported from the TypeScript reference implementation
([tokens-studio/tokenscript-interpreter](https://github.com/tokens-studio/tokenscript-interpreter))
and at first was checked against the cross-runtime compliance suite
([tokens-studio/tokenscript-compliance-suite](https://github.com/tokens-studio/tokenscript-compliance-suite)).
The suite has no license, so it was never vendored: `bb compliance:fetch`
checked it out at a pinned commit (`66edb91`, with the color
specifications of the TypeScript interpreter at `ad4142f`), and a test
ran it when the checkout was there.

In October 2026 we dropped it, for three reasons:

- The public suite is old and no longer maintained. Its successor is
  proprietary, so there is no current version to follow.
- It compared error messages, so the interpreter had to carry the
  English messages of the TypeScript implementation. The interpreter
  now reports errors as codes and data only, and callers such as Penpot
  write their own (translated) messages, see
  [Errors and issues](errors.md).
- Some of its expectations disagreed with the reference interpreter we
  reproduce (the cases listed above), so the run needed a list of
  known failures.

Its cases were rewritten as `tokenscript.language-test` in our own
words and grouping: 187 of its 206 cases, without the duplicates and
the cases our other tests already cover. Errors are asserted by code,
and the disagreements above by our behaviour. These tests are now the
reference for the language; the suite is not used, fetched or copied
from anymore.
