<!-- README.md is generated from README.Rmd. Please edit that file and knit -->



[![Project Status: Concept – Minimal or no implementation has been done yet, or the repository is only intended to be a limited example, demo, or proof-of-concept.](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
[![Build Status](https://travis-ci.com/ropensci/staypuft.svg?branch=master)](https://travis-ci.com/ropensci/staypuft)

`staypuft` is a port of Python's [marshmallow][] for converting objects to and from R data structures 

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
  name = Character$new(),
  title = Character$new()
)
z
#> <schema: MySchema>
#> fields: name, title
x <- list(name = "Jane Doe", title = "Howdy doody")
z$load(x)
#> $name
#> [1] "Jane Doe"
#> 
#> $title
#> [1] "Howdy doody"
z$load_json(jsonlite::toJSON(x, auto_unbox=TRUE))
#> $name
#> [1] "Jane Doe"
#> 
#> $title
#> [1] "Howdy doody"
```

## Meta

* Please [report any issues or bugs](https://github.com/ropensci/staypuft/issues).
* License: MIT
* Get citation information for `staypuft` in R doing `citation(package = 'staypuft')`
* Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.

[![rofooter](https://ropensci.org/public_images/github_footer.png)](https://ropensci.org)


[marshmallow]: https://github.com/marshmallow-code/marshmallow/
