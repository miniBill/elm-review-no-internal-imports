# elm-review-no-internal-imports

Provides [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rules to enforce boundaries in projects that vendor code.

## Provided rules

- [`NoInternalImports`](https://package.elm-lang.org/packages/miniBill/elm-review-no-internal-imports/1.0.0/NoInternalImports) - Reports boundary violations for projects with vendored code..

## Configuration

```elm
module ReviewConfig exposing (config)

import NoInternalImports
import Review.Rule exposing (Rule)

config : List Rule
config =
    [ NoInternalImports.rule []
    ]
```

You must pass as parameter all the `source-directories` that are _outside_ your project's root (that is, the ones that start with `..` or `/`).

Package boundaries are automatically detected by searching for `elm.json` files one level up from the directories ending with `/src`.

The packages don't need to be published, so you can separate a larger app into "packages" by simply creating the necessary `elm.json` files. The rule extracts the exposed modules from there and prevents importing of anything else.

## Try it out

You can try the example configuration above out by running the following command:

```bash
elm-review --template miniBill/elm-review-no-internal-imports/example
```
