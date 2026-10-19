# Agent guide

Instructions for coding agents working in this repository. The
library is described in [README.md](README.md) and [docs/](docs/README.md);
commit and pull request rules are in [CONTRIBUTING.md](CONTRIBUTING.md).

## Commands

```bash
bb test                       # JVM and ClojureScript tests
bb test:jvm lexer processor   # JVM, only the given test namespaces
bb test:cljs                  # ClojureScript on node
bb lint                       # clj-kondo and cljfmt check
bb fmt                        # cljfmt fix
```

All code is `.cljc` and must behave the same on the JVM and in
ClojureScript: run both test tasks and `bb lint` before you finish.

## Tests are the reference, not the compliance suite

The tests in `test/tokenscript/` define how the language behaves;
`tokenscript.language-test` covers it end to end. The tokens-studio
compliance suite is no longer used. It is outdated, its successor is
proprietary, and it has no license. See "History: why not the
compliance suite" in [docs/testing.md](docs/testing.md).

- Do not bring the suite back: no fetch task, no checkout, no
  `TOKENSCRIPT_COMPLIANCE_DIR` or similar variables, no runner, no CI
  step.
- Do not copy cases, fixtures or expected outputs from the suite or its
  successor, and do not change behaviour or tests to match them.
- For new or changed behaviour, add cases to
  `tokenscript.language-test` (or the namespace of the area), written
  by us, asserting the printed value with its type or the error code.
- Do not change the outputs pinned in docs/testing.md (`7` not `7.0`,
  `round(2.5)` is `3`, ...) unless asked to.

## Errors are data

Errors carry a code and data, never messages
([docs/errors.md](docs/errors.md)). Callers such as Penpot write the
messages.

- Raise with `tokenscript.errors/raise` and a code. Put values in the
  data (names, types, units, numbers), and keywords where the reason
  varies (`:reason`, `:constraint`), never English sentences.
- Wrap a caught error with `errors/cause-data`.
- Tests assert error codes and data, never message strings.

## Docs

When behaviour, an error code or its data changes, update the matching
page under `docs/` in the same change.
