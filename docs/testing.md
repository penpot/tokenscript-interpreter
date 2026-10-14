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

CI runs `bb lint`, and the tests with the compliance suite on both
platforms, for every pull request.

## Compliance suite

The cross-runtime compliance suite
([tokens-studio/tokenscript-compliance-suite](https://github.com/tokens-studio/tokenscript-compliance-suite))
is not vendored. `tokenscript.compliance-test` runs it when
`TOKENSCRIPT_COMPLIANCE_DIR` points at a checkout, and skips it
otherwise.

`bb compliance:fetch` checks out the suite and the TypeScript
interpreter (for its color specifications) into `target/compliance/`,
at the commits pinned in `bb.edn`. The test tasks then set the
variables themselves. To run against other checkouts, set them:

```bash
TOKENSCRIPT_COMPLIANCE_DIR=/path/to/compliance-suite \
TOKENSCRIPT_SPECIFICATIONS_DIR=/path/to/typescript-interpreter/data/specifications \
  bb test:jvm compliance
```

Without `TOKENSCRIPT_SPECIFICATIONS_DIR`, the color specification files
the suite references are read from `../specifications` next to the
suite.

## Known divergences

The implementation reproduces the TypeScript reference interpreter.
The suite expects a different behaviour in a few cases, listed in
`tokenscript.compliance-test/known-divergences`:

- `4 + 2.5 + 3 - 2.5` renders as `7`, the suite expects `7.0`.
- `round(2.5)` is `3` (JavaScript rounding), the suite expects `2`.
- Strings inside lists render without quotes.
- `type(#333)` is `color.hex`, the suite expects `hex`.
- Null colors render as an empty string and null units as `nullpx`.
- Empty lists render as an empty string, the suite expects `[]`.
