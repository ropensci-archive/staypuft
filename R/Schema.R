#' @title Schema
#' @description Base schema class with which to define custom schemas
#'
#' @export
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
#'
#' # list of lists
#' z <- Schema$new("MySchema",
#'   name = puft_fields$character(),
#'   title = puft_fields$character()
#' )
#' x <- list(
#'   list(name = "Jane Doe", title = "hello world"),
#'   list(name = "Alice Water", title = "bye mars")
#' )
#' z$load(x, many = TRUE)
#' # z$load(x, many = FALSE)
#' 
#' # data.frame's
#' x <- data.frame(name = "jill", title = "boss", stringsAsFactors = FALSE)
#' x2 <- data.frame(name = c("jill", "jane"), title = c("boss", "ceo"),
#'   stringsAsFactors = FALSE)
#' x2 <- data.frame(name = c("jill", "jane"), title = c("boss", "ceo"),
#'   stringsAsFactors = FALSE)
#' z <- Schema$new("FooBar",
#'   name = puft_fields$character(),
#'   title = puft_fields$character()
#' )
#' z$load_df(x)
#' z$load_df(x2)
#' z$load_df(x2, many = TRUE, simplifyVector = FALSE)
Schema <- R6::R6Class("Schema",
  public = list(
    #' @field schema_name the schema name
    schema_name = NULL,
    #' @field fields field names
    fields = list(),

    #' @description Create a new `Schema` object
    #' @param schema_name (character) the schema name
    #' @param ... additional arguments, passed to `fields`
    #' @return A new `Schema` object
    initialize = function(schema_name, ...) {
      self$schema_name <- schema_name
      self$fields <- list(...)
    },

    #' @description print method for `Schema` objects
    #' @param x self
    #' @param ... ignored
    print = function(x, ...) {
      cat(glue::glue("<schema: {self$schema_name}>"), sep = "\n")
      cat(glue::glue("fields: {paste0(names(self$fields), collapse=', ')}"),
        sep = "\n")
    },

    #' @description Convert various objects to a list
    #' @param x input
    #' @return list
    dump = function(x) {
      # assert(x, "list")
      x <- private$hndlr(x)
      for (i in seq_along(x)) {
        if (!names(x)[i] %in% names(self$fields))
          stop("named element not in allowed set",
            call. = FALSE)
      }

      as.list(x)
    },

    #' @description Same as `dump()`, but returns JSON
    #' @param x input
    #' @param ... additional params passed to [jsonlite::toJSON()]
    #' @return JSON (character)
    dump_json = function(x, ...) {
      jsonlite::toJSON(self$dump(x), ...)
    },

    #' @description Load data
    #' @param data a named list
    #' @param many (logical) Should be set to `TRUE` if `obj` is a list of
    #' lists. default: `FALSE`
    #' @param partial (logical) not implemented yet
    #' @param unknown (character) one or "raise", "exclude", or "include".
    #' default: "raise"
    #' @param as_df (logical) convert to tibble? default: `FALSE`
    #' @return xxxx
    load = function(data, many = FALSE, partial = FALSE, unknown = "raise",
      as_df = FALSE) {

      must_include(unknown, c('raise', 'exclude', 'include'))
      parse_one <- function(data, self, miss_ing, partial) {
        if (!has_names(data))
          stop("all elements in a list must be named", call. = FALSE)
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
        return(ret)
      }
      if (!many) {
        ret <- parse_one(data, self, miss_ing, partial)
      } else {
        ret <- lapply(data, parse_one, self = self, miss_ing = miss_ing,
          partial = partial)
      }

      # handle missing
      if (unknown != "exclude") {
        # uknown_fields <- names(data)[!names(data) %in% names(self$fields)]
        missing_one <- function(z, ret, self, unknown) {
          unk <- z[which(!names(z) %in% names(self$fields))]
          for (i in seq_along(unk)) {
            key <- names(unk)[i]
            if (unknown == "include") {
              ret[[ key ]] <- unk[[i]]
            } else {
              stop("Unknown field: ", key, call. = FALSE)
            }
          }
          return(ret)
        }
        if (!many) {
          ret <- missing_one(data, ret, self, unknown)
        } else {
          for (i in seq_along(ret)) {
            ret[[i]] <- missing_one(data[[i]], ret[[i]], self, unknown)
          }
        }
      }

      if (as_df) as_tbl(ret, many) else ret
    },

    #' @description Same as `load()`, but takes JSON as input
    #' @param data a named list
    #' @param many (logical) Should be set to `TRUE` if `obj` is a list of
    #' lists. default: `FALSE`
    #' @param partial (logical) not implemented yet
    #' @param unknown (character) one or "raise", "exclude", or "include".
    #' default: "raise"
    #' @param ... additional params passed to [jsonlite::fromJSON()]
    #' @return a list
    load_json = function(data, many = FALSE, partial = FALSE,
      unknown = "raise", ...) {

      self$load(jsonlite::fromJSON(data, ...), many = many, partial = partial,
        unknown = unknown)
    },

    #' @description Same as `load()`, but takes a data.frame as input
    #' @param data a data.frame
    #' @param many (logical) Should be set to `TRUE` if `obj` is a list of
    #' lists. default: `FALSE`
    #' @param partial (logical) not implemented yet
    #' @param unknown (character) one or "raise", "exclude", or "include".
    #' default: "raise"
    #' @param ... additional params passed to [jsonlite::fromJSON()]
    #' @return a list
    load_df = function(data, many = FALSE, partial = FALSE, unknown = "raise",
      ...) {

      self$load_json(jsonlite::toJSON(x), many = many, partial = partial,
        unknown = unknown, ...)
    }
  ),

  private = list(
    hndlr = function(x) {
      clz <- private$o_type(x)
      switch(clz,
        base = x,
        S3 = unclass(x),
        S4 = x,
        R6 = {
          flds <- names(self$fields)
          out <- list()
          for (i in seq_along(flds)) out[[ flds[i] ]] <- x[[flds[i]]]
          out
        },
        RC = x
      )
    },
    o_type = function(x) {
      if (!is.object(x)) {
        "base"
      } else if (!isS4(x) && !is.environment(x)) {
        "S3"
      } else if (!inherits(x, "refClass") && !is.environment(x)) {
        "S4"
      } else if (is.environment(x)) {
        "R6"
      } else {
        "RC"
      }
    }
  )
)

as_tbl <- function(x, many = FALSE) {
  chek_for_pkg('tibble')
  if (many) return(do.call(rbind, lapply(x, tibble::as_tibble)))
  return(tibble::as_tibble(x))
}
