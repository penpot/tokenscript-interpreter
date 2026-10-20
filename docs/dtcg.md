# Token files

`tokenscript.dtcg` reads and writes design token files:

- DTCG files, from the 2022 drafts to the
  [Format Module 2025.10](https://www.designtokens.org/tr/2025.10/format/)
  with the [Color Module 2025.10](https://www.designtokens.org/tr/2025.10/color/).
- Tokens Studio's legacy files, with `value` and `type` instead of
  `$value` and `$type`.
- Tokens Studio's multi-set files, with `$themes` and `$metadata`.

Documents are decoded JSON, maps with string keys, on both sides.
Decoding and encoding the text is up to the caller.

## Reading

```clojure
(require '[tokenscript.core :as ts]
         '[tokenscript.dtcg :as dtcg])

(def doc
  (dtcg/read {"space" {"$type" "dimension"
                       "base" {"$value" {"value" 8 "unit" "px"}}
                       "double" {"$value" "{space.base} * 2"}}}
             {:set-name "core"}))

(:sets doc)
;; => [{:name "core"
;;      :tokens {"space.base" {:value "8px" :type "dimension"}
;;               "space.double" {:value "{space.base} * 2" :type "dimension"}}
;;      :groups {} :issues []}]

(ts/build (:tokens (first (:sets doc))))
```

| Key | |
|---|---|
| `:document` | `:single-set` or `:multi-set`. A file is multi-set when it has `$themes` or `$metadata`, or with the option `:multi-set? true`. |
| `:sets` | `[{:name :tokens :groups :issues}]`, in the order of `tokenSetOrder`, then the order of the file. |
| `:themes` | `[{:id :name :group :description :is-source :sets}]`; `:sets` maps a set name to `:enabled`, `:source` or `:disabled`. |
| `:active-themes`, `:active-sets` | From `$metadata`, when it has them. |
| `:issues` | Every issue of every set. |

Token data has `:value` and `:type`, which the [resolver](resolver.md)
reads, and `:description`, `:deprecated`, `:extensions` (the
`$extensions` object) and `:properties` (other `$` keys) when the file
has them. `:groups` maps a group path to the same properties of the
group.

| Option | |
|---|---|
| `:set-name` | The set name of a single set file. |
| `:multi-set?` | Read the top level keys as sets even without `$themes` and `$metadata`. |
| `:original-type-namespace` | See [writing](#writing). |

### Tokens and groups

The format is decided per node, so a file can mix them:

- A map with `$value` is a token, as is a map with `$ref`, a JSON pointer
  alias, and a map with `value` and `type` (legacy). Every other map
  is a group.
- The type is `$type`, else `type` (legacy), else the `$type` of the
  nearest group, else the type of the token that the value references
  when the value is a single reference (`{color.blue}`). A token left
  without a type is dropped with `:dtcg/missing-type`.
- The DTCG names `fontFamily`, `fontWeight` and `boxShadow` are read as
  `fontFamilies`, `fontWeights` and `shadow`, the names of the bundled
  [token types](validation.md#bundled-token-types).
- `$root` is the token of its group's name: `color.accent.$root` is the
  token `color.accent`.
- `$extends` merges the tokens of the group it names into the group;
  the group's own tokens win, whole.
- A `$ref` to a token's value (`#/colors/blue/$value`) is the reference
  `{colors.blue}`. Pointers into a value are not supported.

### Values

| In the file | In the token data |
|---|---|
| strings, numbers, booleans | kept |
| `{"value": 16, "unit": "px"}` | `"16px"` |
| `{"colorSpace": .., "components": .., "alpha": .., "hex": ..}` | a hex color (`#rrggbbaa` with alpha): `hex` when present, else converted from the components |
| typography, other composites | the same map; its fields are read like values |
| a shadow object or list | a vector of shadow objects with `offsetX`, `offsetY`, `blur`, `spread`, `color` and `inset` (a boolean). Tokens Studio's `x`, `y` and `type: innerShadow` are read as `offsetX`, `offsetY` and `inset` |

Color components are converted for `srgb`, `srgb-linear`, `hsl` (with
saturation and lightness in percent, as in the Color Module), `oklab`,
`oklch` and `xyz-d65`; `none` is 0. The bundled schemas have no
conversion to hex for the other color spaces, and the one for Display
P3 does no gamut mapping, so a color in them needs a `hex` or fails the
token with `:dtcg/invalid-value`.

### Issues

Issues have the shape of [validation issues](errors.md#issues), with
the set name in `:set` and the token name in `:data`:

| Code | Severity | |
|---|---|---|
| `:dtcg/invalid-value` | error | The value cannot be read; the token is dropped. `:reason` is `:unsupported-color-space`, `:invalid-components`, `:invalid-alpha` or `:unsupported-pointer`. |
| `:dtcg/missing-type` | warning | No type can be found; the token is dropped. |
| `:dtcg/name-collision` | warning | A token's name is also the group of another token (`a` and `a.b`). Both are kept. |
| `:dtcg/invalid-node` | warning | A value in a group that is neither a token nor a group. |
| `:dtcg/invalid-name` | error | `$root` at the top of a file. |
| `:dtcg/invalid-extends`, `:dtcg/circular-extends` | error | `$extends` names no group, or groups extend each other. |

A document that is not a JSON object raises `:dtcg/invalid-document`.

## Writing

`(dtcg/write document opts)` writes a document of the shape `read`
returns as one multi-set file; `write-files` writes one file per set,
`$themes.json` and `$metadata.json`, keyed by file name. A token whose
name is also a group is written as the group's `$root`.

| Option | |
|---|---|
| `:profile :tokens-studio` | The default: values as given, the type names as given, `$description` when the token has one. |
| `:profile :dtcg` | DTCG 2025.10: literal dimensions as `{value, unit}` objects (`px` when there is no unit), hex colors as `srgb` color objects with `hex`, numeric font weights, numbers and opacities as numbers, typography fields under their DTCG names, and the type names below. References and expressions stay strings. |
| `:original-type-namespace` | With `:dtcg`, an `$extensions` namespace that keeps the type of a token whose DTCG type differs (`spacing` is written as `dimension`). `read` with the same option restores the type and takes the namespace out of the extensions. |

| Type | DTCG 2025.10 type |
|---|---|
| `sizing`, `spacing`, `borderRadius`, `borderWidth`, `fontSizes`, `letterSpacing` | `dimension` |
| `lineHeights` | `number` |
| `fontFamilies`, `fontWeights` | `fontFamily`, `fontWeight` |
| `boxShadow` | `shadow` |
| others | unchanged |

`$themes` and `$metadata` are written in both profiles: the Format
Module has no sets, and the
[Resolver Module](https://www.designtokens.org/tr/drafts/resolver/)
that will have them is a draft.
