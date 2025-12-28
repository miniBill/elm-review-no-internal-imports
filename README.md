# elm-review-no-internal-imports

Provides [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rules to REPLACEME.

## Provided rules

- [`NoInternalImports`](https://package.elm-lang.org/packages/miniBill/elm-review-no-internal-imports/1.0.0/NoInternalImports) - Reports REPLACEME.

## Configuration

```elm
module ReviewConfig exposing (config)

import NoInternalImports
import Review.Rule exposing (Rule)

config : List Rule
config =
    [ NoInternalImports.rule
    ]
```

## Try it out

You can try the example configuration above out by running the following command:

```bash
elm-review --template miniBill/elm-review-no-internal-imports/example
```
