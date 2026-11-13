# Token files

`tokenscript.document.read` and `tokenscript.document.write` read and
write design token files:

- DTCG files, from the 2022 drafts to the
  [Format Module 2025.10](https://www.designtokens.org/tr/2025.10/format/)
  with the [Color Module 2025.10](https://www.designtokens.org/tr/2025.10/color/).
- Legacy files, with `value` and `type` instead of `$value` and
  `$type`.
- Multi-set files, with `$themes` and `$metadata`.

Documents are decoded JSON, maps with string keys, on both sides.
Decoding and encoding the text is up to the caller. DTCG and legacy
tokens are in `tokenscript.document.dtcg`, sets and themes in
`tokenscript.document.themes`.

## Reading

```clojure
(require '[tokenscript.core :as ts]
         '[tokenscript.document.read :as read])

(def doc
  (read/read {"space" {"$type" "dimension"
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

### Tokens and groups

The format is decided per node, so a file can mix them:

- A map with `$value` is a token, as is a map with `value` and `type`
  (legacy) and a map with a `$ref` and no tokens or groups of its own
  (a JSON pointer alias). Every other map is a group.
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
  the group's own tokens win, whole. A `$ref` of a group, its JSON
  Schema form, does the same: on a group with tokens of its own, or
  pointing at a group (`"button-primary": {"$ref": "#/button"}`).
- A `$ref` to a token or its value (`#/colors/blue`,
  `#/colors/blue/$value`) is the reference `{colors.blue}`, also inside
  a composite value (`"fontFamily": {"$ref": "#/base/font/$value"}`).
  Pointers into a value (`#/colors/blue/$value/components/0`) fail the
  token.
- A token named below another token (`a` and `a.b`, as `$root` gives)
  is read; the [resolver](resolver.md) reports it.

### Value normalization

| In the file | In the token data |
|---|---|
| strings, numbers, booleans | kept |
| `{"value": 16, "unit": "px"}`, `{"value": 200, "unit": "ms"}` (dimensions, durations) | `"16px"`, `"200ms"` |
| `{"colorSpace": .., "components": .., "alpha": .., "hex": ..}` | the color in its color space, `oklch(0.628, 0.2577, 29.23, 0.5)`, see [colors](#colors) |
| typography, other composites | the same map; its fields are read like values |
| a shadow object or list | a vector of shadow objects with `offsetX`, `offsetY`, `blur`, `spread`, `color` and `inset` (a boolean). The legacy `x`, `y` and `type: innerShadow` are read as `offsetX`, `offsetY` and `inset` |

### Colors

A color object is read as the color function of its color space in the
bundled schemas, with the alpha as fourth argument: `srgb`,
`srgb-linear` (`linearsrgb`), `hsl` (saturation and lightness in
percent, as in the Color Module), `lab`, `lch`, `oklab`, `oklch`,
`display-p3` (`p3`), `xyz-d65` and `xyz-d50`; `none` is 0. The
resolver converts them with the schemas, like any color token. An
`srgb` color whose `hex` is exactly its components is read as the hex.

The schemas have no color space for `a98-rgb`, `prophoto-rgb` and
`rec2020`, and read `hwb` whiteness and blackness as fractions where
DTCG has percentages. A color in them is read as its `hex` with
`:dtcg/unsupported-color-space`, and fails the token without one.

### Validations

Issues have the shape of [validation issues](errors.md#issues), with
the set name in `:set` and the token name in `:data`:

| Code | Severity | |
|---|---|---|
| `:dtcg/invalid-value` | error | The value cannot be read; the token is dropped. `:reason` is `:unsupported-color-space`, `:invalid-components`, `:invalid-alpha` or `:unsupported-pointer` (with the `:pointer`, and the field `:path` inside a composite value). |
| `:dtcg/missing-type` | warning | No type can be found; the token is dropped. |
| `:dtcg/unsupported-color-space` | warning | A color in a color space the schemas don't have is read as its `hex`. `:data` has the `:color-space` and the field `:path`. |
| `:dtcg/invalid-node` | warning | A value in a group that is neither a token nor a group. |
| `:dtcg/invalid-name` | error | `$root` at the top of a file. |
| `:dtcg/invalid-extends`, `:dtcg/circular-extends` | error | `$extends` is not a reference or names no group, or groups extend each other. `:data` has the `:group` and its `:extends`. |

A document that is not a JSON object raises `:dtcg/invalid-document`.

## Writing

`(write/write document opts)` writes a document of the shape
`read/read` returns as one multi-set file; `write-files` writes one file per set,
`$themes.json` and `$metadata.json`, keyed by file name. A token whose
name is also a group is written as the group's `$root`.

| Option | |
|---|---|
| `:profile :default` | The default: values as given, the type names as given, `$description` when the token has one. |
| `:profile :dtcg` | DTCG 2025.10: literal dimensions as `{value, unit}` objects (`px` when there is no unit), colors as color objects in their color space (`srgb` for hex and HSL colors) with the `hex` the schemas convert them to, numeric font weights, numbers and opacities as numbers, typography fields under their DTCG names, and the type names below. References and expressions stay strings. |

| Type | DTCG 2025.10 type |
|---|---|
| `sizing`, `spacing`, `borderRadius`, `borderWidth`, `fontSizes`, `letterSpacing` | `dimension` |
| `lineHeights` | `number` |
| `fontFamilies`, `fontWeights` | `fontFamily`, `fontWeight` |
| `boxShadow` | `shadow` |
| others | unchanged |

A `:dtcg` file reads back with these types and values, not the
document it was written from: `spacing` comes back as `dimension`, `4`
as `4px`.

`$themes` and `$metadata` are written in both profiles: the Format
Module has no sets, and the
[Resolver Module](https://www.designtokens.org/tr/drafts/resolver/)
that will have them is a draft.
