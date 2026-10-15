# Token validation

A token type schema checks the resolved value of every token of its
type. A failed check adds a warning to the token's issues; the token
keeps its value.

`default-config` validates the [bundled token types](#bundled-token-types).
Register more token type schemas to validate other types, or to replace
a bundled one.

## Bundled token types

The names are the token types of DTCG and Tokens Studio files. A token
with another type, or without one, is not validated.

| Type | Valid values | Codes |
|---|---|---|
| `dimension`, `sizing`, `spacing`, `borderRadius`, `fontSizes`, `letterSpacing`, `lineHeights`, `rotation` | A number, with or without a unit | `INVALID_<TYPE>_TYPE` (`INVALID_DIMENSION_TYPE`, `INVALID_SIZING_TYPE`, ..., `INVALID_FONT_SIZE_TYPE`, `INVALID_LINE_HEIGHT_TYPE`) |
| `borderWidth` | Like `dimension`, not negative | `INVALID_BORDER_WIDTH_TYPE`, `INVALID_BORDER_WIDTH_NEGATIVE` |
| `number` | A number without a unit | `INVALID_NUMBER_TYPE`, `INVALID_NUMBER_UNIT` |
| `opacity` | A number from 0 to 1, or from `0%` to `100%` | `INVALID_OPACITY_TYPE`, `INVALID_OPACITY_RANGE` |
| `color` | A color in any color space | `INVALID_COLOR_TYPE` |
| `textCase` | `none`, `uppercase`, `lowercase` or `capitalize`, in any letter case | `INVALID_TEXT_CASE_VALUE` |
| `textDecoration` | A string; which keywords are allowed is up to the caller | `INVALID_TEXT_DECORATION_TYPE` |
| `fontWeights` | A number from 1 to 1000, a name (`bold`) or words (`700 italic`) | `INVALID_FONT_WEIGHT_TYPE`, `INVALID_FONT_WEIGHT_RANGE` |
| `fontFamilies` | A name or a list of names | `INVALID_FONT_FAMILY_TYPE` |
| `typography` | A dictionary; its fields by their types (below) | `INVALID_TYPOGRAPHY_TYPE` |
| `shadow` | A list of dictionaries; their fields by their types (below) | `INVALID_SHADOW_TYPE` |

The fields of a typography are `fontFamily`, `fontSize`, `fontWeight`,
`letterSpacing`, `lineHeight`, `paragraphSpacing`, `paragraphIndent`,
`textCase` and `textDecoration`; those of a shadow `color`, `offsetX`,
`offsetY`, `blur` and `spread`. A failing field reports the code of its
type with the field's `:path`: `INVALID_DIMENSION_TYPE` at `[0 "blur"]`.

The checks are about the kind of value, so `1foo` (the number 1 and the
word foo, an implicit list) is not a dimension. Units, ranges and
keywords beyond these are up to the caller: which units a dimension may
have, or the font weight names it knows.

## Token type schemas

```clojure
(require '[tokenscript.core :as ts]
         '[tokenscript.config :as config]
         '[tokenscript.processor :as processor])

(def cfg
  (config/register-schemas
   (ts/default-config)
   [{:uri "/api/v1/core/opacity/0/"
     :schema {:name "opacity"
              :type "token"
              :validation {:script "if ({input} < 0 || {input} > 1) [ return \"INVALID_OPACITY_RANGE\"; ]; return true;"}}}]))

(:issues (processor/build {"faded" {:value "1.5" :type "opacity"}} {:config cfg}))
;; => {"faded" [{:type :tokenscript/issue :code "INVALID_OPACITY_RANGE" :severity :warning ...}]}
```

| Key | |
|---|---|
| `name` | The token type, matched case insensitively against the `:type` of the token data. |
| `validation.script` | The validation script. |
| `schema` | The shape of structured values, for [nested validation](#nested-values). |

## Validation scripts

The script gets the resolved value as `{input}` and returns `true` when
it is valid, or an error code string. Codes follow
`INVALID_<TYPE>_<REASON>`.

Other results become codes too:

| Result | Code |
|---|---|
| anything but `true` or a string | `VALIDATION_INVALID_RESULT` |
| the script raises | `VALIDATION_ERROR: <message>` |
| the script does not parse | `VALIDATION_ERROR: invalid validation script: <script>` |

`token-types/validation-error-types` lists the codes the scripts of a
registry can return, read from their `return "..."` statements.

## Nested values

When the value is valid, its fields are validated by their own types.
`schema` names them with `{"type": "token", "url": <token type URI>}`,
in `properties` for an object or in `items` for a list. Field names
match ignoring case, `-` and `_`, so `lineHeight` also finds
`line-height`:

```json
{
  "name": "shadow",
  "type": "token",
  "schema": {
    "type": "list",
    "items": {
      "type": "object",
      "properties": {
        "color": { "type": "token", "url": "/api/v1/core/color/0/" },
        "blur": { "type": "token", "url": "/api/v1/core/dimension/0/" }
      }
    }
  }
}
```

A failure in a field carries its `:path` (`[0 "blur"]`). A type that
reaches itself again fails with `CIRCULAR_REFERENCE`, and validation
stops at a depth of 10 with `MAX_VALIDATION_DEPTH_EXCEEDED`.

## API

| Function | |
|---|---|
| `validation/validate` | Validates a symbol as a token type. Returns `{:valid? :error :token-type :path}` results. |
| `validation/type-issues` | The failures as issues, as the resolver stores them; see [Errors and issues](errors.md#issues). |
| `token-types/register-validation` | Sets the validation script of a token type in a registry. |

`processor/resolve-value` validates its value when given `:type`.
