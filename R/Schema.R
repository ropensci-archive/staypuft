#' Schema
#' 
#' @export
#' @details
#' **Methods**
#'   \describe{
#'     \item{`dump(x, ...)`}{
#'       xxx
#'     }
#'     \item{`dump_json(x, ...)`}{
#'       xxx
#'     }
#'     \item{`load(x, ...)`}{
#'       xxx
#'     }
#'     \item{`load_json(x, ...)`}{
#'       xxx
#'     }
#'   }
#'
#' @format NULL
#' @usage NULL
#' @examples
#' z <- Schema$new("FooBar",
#'   name = puft_fields$character(),
#'   title = puft_fields$character()
#' )
#' z
#' z$fields
#' names(z$fields)
#' 
#' x <- list(name = "Jane Doe", title = "Howdy doody")
#' x
#' z$dump(x)
#' z$dump_json(x)
#' z$dump_json(x, auto_unbox = TRUE)
#' 
#' 
#' z <- Schema$new("MySchema",
#'   name = puft_fields$character(),
#'   title = puft_fields$character()
#' )
#' z
#' x <- list(name = "Jane Doe", title = "Howdy doody")
#' z$load(x)
#' z$load_json(jsonlite::toJSON(x, auto_unbox=TRUE))
#' 
#' # unknown field
#' # x <- list(name = "Jane Doe", my_title = "Howdy doody")
#' # z$load(x)
#' # z$load_json(jsonlite::toJSON(x, auto_unbox=TRUE))
#' 
#' # as data.frame
#' z <- Schema$new("MySchema",
#'   name = puft_fields$character(),
#'   title = puft_fields$character()
#' )
#' x <- list(name = "Jane Doe", title = "hello world")
#' z$load(x, as_df = TRUE)
Schema <- R6::R6Class("Schema",
  public = list(
    schema_name = NULL,
    fields = list(),

    initialize = function(schema_name, ...) {
      self$schema_name <- schema_name
      self$fields <- list(...)
    },

    print = function(x, ...) {
      cat(glue::glue("<schema: {self$schema_name}>"), sep = "\n")
      cat(glue::glue("fields: {paste0(names(self$fields), collapse=', ')}"), 
        sep = "\n")
    },

    dump = function(x) {
      assert(x, "list")
      for (i in seq_along(x)) {
        if (!names(x)[i] %in% names(self$fields))
          stop("named element not in allowed set", 
            call. = FALSE)
      }

      as.list(x)
    },

    dump_json = function(x, ...) {
      jsonlite::toJSON(self$dump(x), ...)
    },

    load = function(data, many = NULL, partial = FALSE, unknown = "raise", 
      as_df = FALSE, ...) {

      must_include(unknown, c('raise', 'exclude', 'include'))
      ret = list()
      for (i in seq_along(self$fields)) {
        key <- names(self$fields)[i]
        fld <- self$fields[[i]]
        raw_value <- data[[key]] %||% miss_ing
        
        if (inherits(raw_value, "Missing")) {
          # FIXME: other logic
          if (partial) next
        }

        # if nested, do something ...

        # deserialize
        if (!inherits(raw_value, "Missing")) {
          val <- fld$deserialize(raw_value, key)
          # append to list
          ret[[ key ]] <- val
        }
      }
     
      # handle missing
      if (unknown != "exclude") {
        # uknown_fields <- names(data)[!names(data) %in% names(self$fields)]
        unk <- data[which(!names(data) %in% names(self$fields))]
        for (i in seq_along(unk)) {
          key <- names(unk)[i]
          if (unknown == "include") {
            ret[[ key ]] <- unk[[i]]
          } else {
            stop("Unknown field: ", key)
          }
        }
      }
      
      if (as_df) as_tbl(ret) else ret
    },

    load_json = function(x, ...) {
      self$load(jsonlite::fromJSON(x, ...))
    }
  )
)

as_tbl <- function(x) {
  chek_for_pkg('tibble')
  tibble::as_tibble(x)
}
