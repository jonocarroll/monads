
<!-- README.md is generated from README.Rmd. Please edit that file -->

# monads <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->
<!-- badges: end -->

The goal of monads is to provide a ‘toy’ implementation of some common
monads.

See [this blog post](https://jcarroll.com.au/2024/10/18/monads-in-r/)
for some additional background.

## Installation

You can install the development version of monads like so:

``` r
# install.packages("remotes")
remotes::install_github("jonocarroll/monads")
```

## Example

With the addition of a new ‘flatMap’ pipe `%>>=%` a monad instance can
be passed to regular functions, unpacked, applied, and re-packaged. One
example is to create a ‘logger’ which records the function applications
at each step.

``` r
library(monads)
library(dplyr, warn.conflicts = FALSE)

result <- loggerM(mtcars) %>>=%
  filter(mpg > 10) %>>=%
  select(mpg, cyl, disp) %>>=%
  arrange(desc(mpg)) %>>=%
  head()

value(result)
#>                 mpg cyl  disp
#> Toyota Corolla 33.9   4  71.1
#> Fiat 128       32.4   4  78.7
#> Honda Civic    30.4   4  75.7
#> Lotus Europa   30.4   4  95.1
#> Fiat X1-9      27.3   4  79.0
#> Porsche 914-2  26.0   4 120.3

logger_log(result)
#> ✔ Log of 4 operations:
#> 
#>  mtcars %>%
#>    filter(mpg > 10) %>%
#>    select(mpg, cyl, disp) %>%
#>    arrange(desc(mpg)) %>%
#>    head()
```

This package also defines `Maybe`, `Result`, `Timer`, and `List` monads
which all work with this new pipe. Additional monads can be added by
third-parties to further extend the capabilities.

See the
[vignette](https://jonocarroll.github.io/monads/articles/monads.html)
for more details.

## Prior Art

- [{monads}](https://github.com/hadley/monads) - a sketched-out
  implementation that relies on dispatch for `flatMap` operations.
- [{rmonad}](https://github.com/arendsee/rmonad) - archived on CRAN, but
  offers a sophisticated ‘funnel’ mechanism and various ways to capture
  steps of a pipeline.
- [{maybe}](https://armcn.github.io/maybe/) - a more detailed
  implementation of `Maybe`.
- [{chronicler}](https://b-rodrigues.github.io/chronicler/) - a way to
  post-process the result at each step and capture information, such as
  the runtime (see `Timer`) or dimensions. Requires an explicit `bind()`
  at each step. [Associated blog
  post](https://www.brodrigues.co/blog/2022-04-11-monads/).
