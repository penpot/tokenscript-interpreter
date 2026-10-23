# Resolver

The resolver (`tokenscript.processor`) resolves tokens whose
values reference each other, from one or several token sets. It never
throws for a bad token: errors are stored as the token's
[issues](errors.md#issues).

## Building

```clojure
(require '[tokenscript.core :as ts]
         '[tokenscript.processor :as processor])

(ts/build {"spacing.base"  {:value "8"}
           "spacing.large" {:value "{spacing.base} * 2"}
           "broken"        {:value "{missing}"}})
;; => {:tokens   {"spacing.base" #Number 8, "spacing.large" #Number 16, "broken" "{missing}"}
;;     :resolved {"spacing.base" #Number 8, "spacing.large" #Number 16, "broken" <error map>}
;;     :issues   {"broken" [<:processor/dependency-error>]}
;;     :graph    {"spacing.base" #{}, "spacing.large" #{"spacing.base"}, "broken" #{"missing"}}
;;     :state    ...}
```

`build` takes a map of token names to token data,
`{:value .. :type .. :metadata ..}`; plain values and `$value` / `$type`
keys work too. Its options are `:config`, `:object-parsers`,
`:token-reader` and `:token-writer`.

### Token reader

The token data is kept as the caller gives it. When a token is parsed,
the `:token-reader` function turns its data into
`{:value .. :type .. :metadata ..}`; the default reads the shapes above.
A caller with its own token records passes them as they are and a reader
for them, and the reader runs only for the tokens an operation parses:

```clojure
(ts/build {"spacing.base" {:val "8" :kind :number :id 1}}
          {:token-reader (fn [{:keys [val kind id]}]
                           {:value val :type (name kind) :metadata id})})
```

The [incremental operations](#incremental-updates) compare token data by
identity first, so records that did not change are skipped without
reading them. A reader can return an error map instead of data
(`errors/error-map`): the token fails with that error and the tokens
referencing it get `:processor/dependency-error`. A `:tokens` entry of
such a token is nil.

### Token writer

The `:token-writer` is the reverse: a function of a token's data as
given, its `:tokens` entry and its issues (nil when clean) that returns
what the caller wants for that token, e.g. its own record with the
resolved value converted. The result then has `:output`, the token names
mapped to what the writer returned. The
[incremental operations](#incremental-updates) run the writer only for
the names in `:changed`; every other `:output` entry is the previous
object, so a caller can keep the result as its cache and compare entries
by identity.

```clojure
(ts/build {"spacing.base" {:val "8" :id 1}}
          {:token-reader (fn [{:keys [val]}] {:value val})
           :token-writer (fn [token value issues]
                           (assoc token :resolved value :errors (mapv :code issues)))})
;; => {... :output {"spacing.base" {:val "8" :id 1 :resolved #Number 8 :errors []}}}
```

Without a writer there is no `:output`.

### Sets

`build` also takes an ordered vector of sets, `[[set-key tokens] ..]`. A
token defined by several sets is taken from the last of them, and
references reach across sets:

```clojure
(def result
  (ts/build [[core-id  {"color.bg" {:value "#fff"} "space" {:value "8"}}]
             [theme-id {"color.bg" {:value "#000"}}]]))

(processor/source result "color.bg")    ;; => theme-id
(processor/metadata result "color.bg")  ;; => the :metadata the reader returned for it
```

Set keys and `:metadata` are not interpreted. A caller can pass its set
ids as keys and its token ids as metadata, and read them back for a
resolved name. A flat map is one set with the key nil.

| Key | |
|---|---|
| `:tokens` | Name to resolved value (a symbol), or the original value for a failed token. |
| `:resolved` | Name to resolved value or error map. |
| `:issues` | Name to a vector of issues; nil when there are none. |
| `:graph` | Name to the set of names it references. |
| `:state` | Input of the [incremental operations](#incremental-updates). |

`processor/tokens->strings` and `processor/tokens->data` convert
`:tokens` to strings or plain data.

## Resolution

A token is parsed in inline mode, and in script mode when it starts with
a statement keyword (see
[Language](language.md#inline-and-script-mode)).
Numbers, booleans, nil and the bare CSS keywords listed in
[Built-in functions](functions.md#other)
are taken as they are.

Resolution is a memoized depth first walk: resolving a token first
resolves everything it references, so the evaluation order comes out of
the recursion. A token that reaches itself gets
`:processor/circular-dependency`, as does every other token on the
cycle; a cycle can also pass through a group or a key of a dictionary
token. A token referencing a failed or missing token gets
`:processor/dependency-error`. Which tokens of a cycle get which of the
two depends on where resolution enters the cycle, so a UI should show
both as a circular reference.

A token name is a dot separated path whose segments contain no
whitespace, braces or brackets. Other names get
`:processor/invalid-token-name` and are still resolved.

### References

| Reference | Value |
|---|---|
| `{color.primary}` | The token `color.primary`. |
| `{color}` | A group: a dictionary of the tokens directly below `color`, keyed by the rest of their name. |
| `{typography.fontSize}` | A field of the structured token `typography`. |
| `{theme.dark}` | A key of the token `theme` whose value is a dictionary; one level deep. |

### Structured tokens

A token whose value is a map or a vector (typography, shadow) is a
structured token. Its string leaves are expressions, resolved like
tokens of their own under field paths such as `shadow[0].blur`. An
error in a field is reported on the token, with the field as `:path`
(`[0 "blur"]`).

`:object-parsers` turns maps into atomic values instead of fields: a
vector of `{:predicate :->symbol}` maps.
`token-data/number-with-unit-parser` reads `{:value 8 :unit "px"}` as
`8px`. There are none by default.

## Incremental updates

These functions take the `:state` of a result and return a new result.
The state they started from is not changed, so a form can preview an
edit on the shared state and drop the result.

| Function | |
|---|---|
| `(create-token state name data opts)` | Adds a token, to the last set by default. Raises `:processor/token-already-exists` when that set has it. |
| `(update-token state name data opts)` | Sets the data of a token, adding it when it does not exist. |
| `(rename-token state name new-name opts)` | Renames a token. References to it break unless `:update-references` rewrites them, see [Renaming](#renaming). |
| `(delete-token state name opts)` | Removes a token. Raises `:processor/token-not-found` when the set does not have it. |
| `(set-sets state sets)` | Replaces the sets, for a theme switch, a toggled set or an edit. |

`opts` is optional. Its `:set` names the set to change; by default it is
the set the token is taken from. An unknown set raises
`:processor/token-set-not-found`.

The token operations add `:dependents` to the result: the dependency
graph between the token and the tokens depending on it. For
`delete-token` it holds the tokens that referenced it. It is empty when
a later set overrides the changed definition, since nothing resolved
changes.

```clojure
(let [{:keys [state]} (ts/build {"a" {:value "8"} "b" {:value "{a} * 2"}})]
  (processor/update-token state "a" {:value "10"}))
;; => {:tokens {"a" #Number 10, "b" #Number 20} :dependents {"a" #{}, "b" #{"a"}} ...}
```

Every operation adds `:changed` to the result: the token names whose
`:tokens`, `:resolved`, `:issues`, `:graph` or `:output` entries it
updated. Every other entry is the one of the previous result, so derived
data per token is best kept in `:output` with a
[token writer](#token-writer), which runs for these names only. The
result maps are updated, not built again.

`set-sets` lets a caller pass its sets as they are after every change.
A set whose map is the same object as before is not looked into. In a
set whose map changed, only the token data that is not the same object
counts as changed. Added and removed sets change the names they define.
When sets move relative to each other, or a set key repeats, every name
is compared.

An operation re-parses only the tokens whose data changed and
re-resolves only what can observe them: the tokens referencing them,
directly or through a group (`{color}`) or a key of a dictionary token
(`{theme.dark}`), found through the reverse dependency graph the state
keeps. A missing token is in that graph too, so creating it fixes the
tokens waiting for it. Every other value is kept.

`affected-tokens`, `modified-dependents`, `broken-references`,
`tokens-with-issues`, `token-error`, `errors` and `issue-with-code?`
read a result. `input` returns the token data a name is taken from, as
the caller gave it.

## Renaming

`rename-token` renames the definition in the `:set` of `opts`, by
default the set the token is taken from. A definition of the old name in
an earlier set, shadowed until then, becomes visible:

```clojure
(def state
  (:state (ts/build [[:a {"token-foo" {:value "red"} "accent" {:value "{token-foo}"}}]
                     [:b {"token-foo" {:value "blue"} "accent" {:value "{token-foo}"}}]])))

(processor/tokens->strings (processor/rename-token state "token-foo" "token-bar"))
;; => {"token-foo" "#FF0000", "token-bar" "#0000FF", "accent" "#FF0000"}
```

The references to the old name then resolve to that definition, or
break when there is none, unless `:update-references` rewrites them:

| `:update-references` | Rewrites the references in |
|---|---|
| `:visible` | The visible definition of every token. |
| `:all` | Every definition of every set, also the shadowed ones. |

References to a field or key of the token (`{typo.fontSize}`) are
rewritten too, as are the string leaves of structured values, but not
references to a token of its own below the name (`{a.x}` when renaming
`a`). Renaming a shadowed definition rewrites nothing: the references
are to the visible one.

The token data is rewritten in its shape: a plain value stays plain and
DTCG data keeps its `$value` key. Records read by a `:token-reader` of
the caller cannot be rewritten by the resolver, and a rewrite raises
`:processor/token-not-updatable`. `(processor/reference-updates state
name new-name opts)` takes the same options and returns the rewritten
values, as the reader returns them, by set key and token name:

```clojure
(processor/reference-updates state "token-foo" "token-bar" {:update-references :all})
;; => {[:a "accent"] "{token-bar}", [:b "accent"] "{token-bar}"}
```

A caller keeping its own records writes them and the new name into its
records, and passes its sets to `set-sets`.

## Previews

`(processor/resolve-value state value opts)` evaluates an expression
against the resolved tokens, for previews while the user types. It
does not change the state and returns `{:value symbol-or-nil :issues
[..]}`; errors are returned as issues, not raised. With `{:type
token-type}` the value is also validated.

The value is parsed in script mode, so unquoted dotted words such as
`http://foo.bar` fail here while the same token value resolves in a
build.

## Caches

Caches that live across builds, all keyed by inputs that fully
determine the result:

- Parsed token values, keyed by the value text (up to 50 000 entries).
  Every build parses all tokens, but on a set or theme switch most
  values were parsed before.
- URI resolution and conversion paths between schemas, in the `cache`
  field of each registry, replaced whenever a spec or conversion is
  added.
- Color conversions, for the last config used (up to 10 000 entries).
- Parsed schema URIs.
- The evaluated constants, for the last config used.
