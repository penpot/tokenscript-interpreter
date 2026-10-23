# Errors and issues

Every error of the lexer, parser, interpreter and resolver is an
`ex-info` whose data is an error map. The resolver does not throw: it
stores the same maps as the issues of the failed token, so errors are
plain data on both sides of the API.

Errors carry no messages. The interpreter reports what failed as a
code and its data; the caller renders them in its own words and
language (Penpot maps them to its translated token errors).

## Error maps

```clojure
{:type    :tokenscript/error
 :kind    :interpreter              ; :lexer, :parser, :interpreter or :processor
 :code    :interpreter/unknown-reference
 :data    {:reference "x"}          ; depends on the code
 :line    1
 :token   {:type :reference :value "x" :line 1 :pos 1 :end-pos 2}}
```

The namespace of the code is the area that raised it: `lexer`,
`parser`, `interpreter`, `operations`, `symbols`, `functions`,
`config`, `processor`, `color` or `unit`. The message of the thrown
`ex-info` is the identifier the other tokenscript implementations use
for the code: `:interpreter/unknown-reference` is
`INT_UNKNOWN_REFERENCE`.

The data holds values, never prose: names, types, units, URIs and
numbers, and keywords where the reason varies, such as `:reason` of
`:parser/invalid-syntax` (`:trailing-input`, `:empty-template-reference`,
...) or `:constraint` of `:functions/argument-out-of-range`
(`:positive`, `:between-minus-one-and-one`, ...). An error caused by
another one holds it as `:cause` (an error map) or, for a host
exception, its message as `:error`.

| Function | |
|---|---|
| `errors/raise` | Throws the error for a code: `(raise :processor/token-not-found {:data {:token-name "a"}})`. |
| `errors/error-map` | The error map, without throwing. |
| `errors/error-map?` | Whether a value is an error map. |
| `errors/->error-map` | The error map of an exception or an error map. |
| `errors/cause-data` | `{:cause error-map}` or `{:error message}` for an exception, to wrap it into the data of another error. |

## Issues

`:issues` of a build result maps a token name to a vector of issues of
two kinds:

| Issue | | The token |
|---|---|---|
| Error map (`:type :tokenscript/error`) | The value could not be resolved. | Keeps its original value in `:tokens`. |
| Validation issue (`:type :tokenscript/issue`) | The value failed its [token type](validation.md). | Keeps its resolved value. |

A validation issue:

```clojure
{:type       :tokenscript/issue
 :code       "INVALID_OPACITY_RANGE"      ; returned by the validation script
 :severity   :warning
 :token-name "faded"
 :path       nil                          ; [0 "blur"] for a nested field
 :data       {:token-type "opacity"}}
```

### Resolver errors

`:processor/dependency-error` is the error of a token that references a
failed or missing token. `:chain` in its data lists the token names
down to the failing one and `:cause` holds the error map of the root
cause, so callers can match the root cause's code:

```clojure
{:code :processor/dependency-error
 :data {:token-name "j"
        :chain      ["j" "i" "missing"]
        :cause      {:code :processor/token-not-found :data {:token-name "missing"} ...}}
 ...}
```

`:processor/circular-dependency` names the token in `:tokens`. An
error in a field of a structured token is stored on the token with the
field as `:path`.

A [token reader](resolver.md#token-reader) can fail a token with an
error map of its own code; the tokens referencing it get
`:processor/dependency-error` with that error as the `:cause`.

`rename-token` raises `:processor/token-not-updatable` when it would
[rewrite the references](resolver.md#renaming) in token data read by a
`:token-reader` of the caller; `:token-name` names that token.
