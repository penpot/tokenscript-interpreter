# Importers

`tokenscript.importers.css` imports CSS as tokenscript. The CSS is
tokenized and parsed, not matched: colors, calls and token references
nest as they do in CSS.

```clojure
(require '[tokenscript.importers.css :as css])

(css/linear-gradient->tokenscript
 "linear-gradient(to right, #5500ff, rgb(255 209 102 / 0.6) 35%, #00ff55)")
;; => "linear-gradient(90deg, #5500ff 0%, rgba(255, 209, 102, 0.6) 35%, #00ff55 100%)"

(css/linear-gradient "linear-gradient(to right, red, {color.brand})")
;; => {"kind" "linear"
;;     "angle" "90"
;;     "stops" [{"color" "red" "position" "0%"}
;;              {"color" "{color.brand}" "position" "100%"}]}
```

| Function | |
|---|---|
| `linear-gradient` | The value of a `gradient` token for the CSS (or tokenscript) `linear-gradient()` `text`, with token references kept: the fields of a token form. Nil when it is no linear gradient. |
| `gradient->tokenscript` | Writes such a value as a tokenscript `linear-gradient()`. |
| `linear-gradient->tokenscript` | Both: the tokenscript for a CSS `linear-gradient()`. |

## What is rewritten

The tokenscript `linear-gradient()` function of the
[bundled schemas](schemas.md#css-linear-gradient) reads most CSS as it
is. The importer rewrites what it does not:

- The direction becomes an angle in degrees: `to right` is `90deg`,
  `0.25turn` is `90deg`.
- The CSS color functions `rgb`, `rgba`, `hsl` and `hsla` get commas, and
  their slash alpha syntax, a division in tokenscript, becomes a fourth
  channel of the `rgba` or `hsla` of CSS: `rgb(255 0 0 / 50%)` is
  `rgba(255, 0, 0, 50%)`.
- `transparent`, which tokenscript has no name for, becomes `#00000000`:
  black with no alpha, as CSS defines it.
- Every stop gets a position in percent: the ones CSS gives stops without
  one, clamped to 0%-100%. A color with two positions is two stops and a
  color hint is left out.
- A declaration (`background: linear-gradient(...);`) is read as its
  value.

Colors that are not CSS color functions (`#f00`, `red`, `oklch(...)`, a
reference) and token references anywhere stay as written.

Text that is not a `linear-gradient()` gives nil: radial, conic and
repeating gradients, a stop positioned in pixels, several gradients, an
unbalanced call, or a `currentcolor` stop: it is the color of an
element, which a token has none of.
