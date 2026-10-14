# Contributing

This library follows
[Penpot's contributing guide](https://github.com/penpot/penpot/blob/develop/CONTRIBUTING.md),
including its Code of Conduct and the Developer's Certificate of Origin.

## Pull requests

Before opening one, run `bb lint` and `bb test` with the compliance
suite checked out (`bb compliance:fetch`). See
[docs/testing.md](docs/testing.md).

## Commits

CI checks every commit message against this format:

```
:emoji: <subject>

[body]
```

| Emoji | Use |
|---|---|
| `:bug:` | Bug fix |
| `:sparkles:` | Improvement or enhancement |
| `:tada:` | New feature |
| `:recycle:` | Refactor |
| `:zap:` | Performance improvement |
| `:books:` | Documentation |
| `:wrench:` | Configuration update |
| `:paperclip:` | Other changes, e.g. tests |
| `:fire:` | Removal of code or files |
| `:arrow_up:` / `:arrow_down:` | Dependency update / downgrade |
| `:boom:` | Breaking change |
| `:ambulance:` | Critical bug fix |
| `:construction:` | Work in progress |
| `:construction_worker:` | CI |
| `:lipstick:` | Cosmetic changes |
| `:rewind:` | Revert |

- Imperative mood, capitalized, no period at the end of the subject.
- Subject at most 70 characters, body lines at most 76.
- A blank line between subject and body.
