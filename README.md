<!-- README.md is generated from README.Rmd. Please edit that file and knit -->



[![Project Status: Concept – Minimal or no implementation has been done yet, or the repository is only intended to be a limited example, demo, or proof-of-concept.](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
[![Build Status](https://travis-ci.com/ropensci/staypuft.svg?branch=master)](https://travis-ci.com/ropensci/staypuft)

`staypuft` is a port of Python's [marshmallow][] for converting objects to and from R data structures 

Main `Schema` methods:
- `load`: 'deserialize', or validate and deserialize an input R data structure to an object
- `dump`: 'serialize', or convert any input to R data structures
- `load_json`: same as `load`, but accepts JSON
- `dump_json`: same as `dump`, but returns JSON

## Installation


```r
remotes::install_github("ropensci/staypuft")
```


```r
library("staypuft")
```

## hello world


```r
z <- Schema$new("MySchema",
  name = puft_fields$character(),
  title = puft_fields$character(),
  num = puft_fields$integer()
)
z
#> <schema: MySchema>
#> fields: name, title, num
x <- list(name = "Jane Doe", title = "Howdy doody", num = 5.5)
z$load(data = x)
#> $name
#> [1] "Jane Doe"
#> 
#> $title
#> [1] "Howdy doody"
#> 
#> $num
#> [1] 5.5
z$load(data = x, as_df = TRUE)
#> # A tibble: 1 x 3
#>   name     title         num
#>   <chr>    <chr>       <dbl>
#> 1 Jane Doe Howdy doody   5.5
z$load_json(jsonlite::toJSON(x, auto_unbox=TRUE))
#> $name
#> [1] "Jane Doe"
#> 
#> $title
#> [1] "Howdy doody"
#> 
#> $num
#> [1] 5.5
```

strict mode for integer


```r
z <- Schema$new("MySchema",
  name = puft_fields$character(),
  title = puft_fields$character(),
  num = puft_fields$integer(strict = TRUE)
)
z$fields$num
#> <fields.Integer>
#> default=Missing
#> attribute=none
#> validate=none
#> required=FALSE
#> load_only=FALSE
#> dump_only=FALSE
#> missing=Missing
#> allow_none=FALSE
#> error_messages=required: 'Missing data for required field.'; null: 'Field may not be null.'; validator_failed: 'Invalid value.'; invalid: 'Not a valid integer.'
x <- list(name = "Jane Doe", title = "Howdy doody", num = 5.5)
z$load(data = x)
#> Error in super$fail("invalid"): ValidationError: Not a valid integer.
```

another example


```r
z <- Schema$new("MySchema",
  name = puft_fields$character(),
  title = puft_fields$character(),
  num = puft_fields$integer(),
  uuid = puft_fields$uuid(),
  foo = puft_fields$boolean()
)
x <- list(name = "Jane Doe", title = "Howdy doody", num = 5.5, 
    uuid = "9a5f6bba-4101-48e9-a7e3-b5ac456a04b5",
    foo = TRUE)
z$load(data = x)
#> $name
#> [1] "Jane Doe"
#> 
#> $title
#> [1] "Howdy doody"
#> 
#> $num
#> [1] 5.5
#> 
#> $uuid
#> [1] "9a5f6bba-4101-48e9-a7e3-b5ac456a04b5"
#> 
#> $foo
#> [1] TRUE

# invalid uuid
x$uuid <- "foo-bar"
z$load(data = x)
#> Error in super$fail("invalid_uuid"): ValidationError: Not a valid UUID.

# invalid boolean
x$uuid <- "9a5f6bba-4101-48e9-a7e3-b5ac456a04b5"
x$foo <- "bar"
z$load(data = x)
#> Error in super$fail("invalid"): ValidationError: Not a valid boolean.
```

## Meta

* Please [report any issues or bugs](https://github.com/ropensci/staypuft/issues).
* License: MIT
* Get citation information for `staypuft` in R doing `citation(package = 'staypuft')`
* Please note that this project is released with a [Contributor Code of Conduct][coc]. By participating in this project you agree to abide by its terms.

[![rofooter](https://ropensci.org/public_images/github_footer.png)](https://ropensci.org)


[marshmallow]: https://github.com/marshmallow-code/marshmallow/
[coc]: https://github.com/ropensci/staypuft/blob/master/CODE_OF_CONDUCT.md
