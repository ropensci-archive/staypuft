schema_opts <- list(
  fields = list(),
  additional = list(),
  exclude = list(),
  dateformat = NULL,
  datetimeformat = NULL,
  render_module = NULL,
  ordered = FALSE,
  index_errors = TRUE,
  include = list(),
  load_only = list(),
  dump_only = list(),
  unknown = stop,
  register = TRUE
)

#' @title Schema
#' @description Base schema class with which to define custom schemas
#'
#' @export
#' @examples
#' z <- Schema$new("FooBar",
#'   name = fields$character(),
#'   title = fields$character()
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
#'   name = fields$character(),
#'   title = fields$character()
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
#'   name = fields$character(),
#'   title = fields$character()
#' )
#' x <- list(name = "Jane Doe", title = "hello world")
#' z$load(x, as_df = TRUE)
#'
#' # list of lists
#' z <- Schema$new("MySchema",
#'   name = fields$character(),
#'   title = fields$character()
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
#'   name = fields$character(),
#'   title = fields$character()
#' )
#' z$load_df(x)
#' z$load_df(x2)
#' z$load_df(x2, many = TRUE, simplifyVector = FALSE)
#' 
#' # nested
#' artist_schema <- Schema$new("ArtistSchema",
#'   name = fields$character(),
#'   role = fields$character(),
#'   instrument = fields$character()
#' )
#' album_schema <- Schema$new("AlbumSchema",
#'   title = fields$character(),
#'   release_date = fields$date(),
#'   artist = fields$nested(artist_schema)
#' )
#' artist_schema
#' album_schema
#' bowie <- list(name="David Bowie", role="lead", instrument="voice")
#' album <- list(title="Hunky Dory", release_date="12-17-1971", artist=bowie)
#' album_schema$dump(album)
#' album_schema$load(album)
#' ## many
#' albums <- list(
#'   list(title="Hunky Dory", release_date="12-17-1971", artist=bowie),
#'   list(title="Mars and Venus", release_date="03-05-1974", artist=bowie)
#' )
#' album_schema$dump(albums, many=TRUE)
#' album_schema$load(albums, many=TRUE)
#' ## bad
#' album$artist <- list(stuff = "things")
#' if (interactive()) album_schema$load(album)
#' 
#' # Deserialize/load and create object with post_load
#' z <- Schema$new("ArtistSchema",
#'   name = fields$character(),
#'   role = fields$character(),
#'   instrument = fields$character(),
#'   post_load = {
#'     function(x) structure(x, class = "Artist", attr = "hello")
#'   }
#' )
#' z$post_load
#' w <- list(name="David Bowie", role="lead", instrument="voice")
#' z$load(w)
#' print.Artist <- function(x) {
#'   cat("Artist\n")
#'   cat(sprintf("  name: %s\n", x$name))
#'   cat(sprintf("  role: %s\n", x$role))
#'   cat(sprintf("  instrument: %s\n", x$instrument))
#' }
#' z$load(w)
#' 
#' # from json
#' json <- jsonlite::toJSON(w)
#' z$load_json(json)
#' ## many
#' ww <- list(
#'   list(name="David Bowie", role="lead", instrument="voice"),
#'   list(name="Michael Jackson", role="lead", instrument="voice")
#' )
#' json <- jsonlite::toJSON(ww)
#' z$load_json(json, simplifyVector = FALSE, many = TRUE)
Schema <- R6::R6Class("Schema",
  inherit = SchemaABC,
  public = list(
    #' @field schema_name the schema name
    schema_name = NULL,
    #' @field fields field names
    fields = list(),
    #' @field post_load field names
    post_load = NULL,
    #' @field many xxxx
    many = NULL,
    #' @field only xxxx
    only = NULL,
    #' @field exclude xxxx
    exclude = NULL,
    #' @field ordered xxxx
    ordered = NULL,
    #' @field load_only xxxx
    load_only = NULL,
    #' @field dump_only xxxx
    dump_only = NULL,
    #' @field partial xxxx
    partial = NULL,
    #' @field unknown xxxx
    unknown = NULL,
    #' @field context xxxx
    context = NULL,
    #' @field opts field names
    opts = schema_opts,

    #' @description Create a new `Schema` object
    #' @param schema_name (character) the schema name
    #' @param ... additional arguments, passed to `fields`
    #' @param only Whitelist of the declared fields to select when
    #' instantiating the Schema. If None, all fields are used. Nested fields
    #' can be represented with dot delimiters.
    #' @param exclude Blacklist of the declared fields to exclude
    #' when instantiating the Schema. If a field appears in both `only` and
    #' `exclude`, it is not used. Nested fields can be represented with dot
    #' delimiters.
    #' @param many Should be set to `True` if ``obj`` is a collection
    #' so that the object will be serialized to a list.
    #' @param context Optional context passed to :class:`fields.Method` and
    #' :class:`fields.Function` fields.
    #' @param load_only Fields to skip during serialization (write-only fields)
    #' @param dump_only Fields to skip during deserialization (read-only fields)
    #' @param partial Whether to ignore missing fields and not require
    #' any fields declared. Propagates down to ``Nested`` fields as well. If
    #' its value is an iterable, only missing fields listed in that iterable
    #' will be ignored. Use dot delimiters to specify nested fields.
    #' @param unknown Whether to exclude, include, or raise an error for unknown
    #' fields in the data. Use `EXCLUDE`, `INCLUDE` or `RAISE`.
    initialize = function(schema_name, ..., post_load = NULL, only = NULL,
      exclude = NULL, many = FALSE, context = NULL, load_only = NULL,
      dump_only = NULL, partial = FALSE, unknown = NULL) {

      class_registry$register(schema_name, self)
      self$schema_name <- schema_name
      self$fields <- list(...)
      self$post_load <- post_load
      self$many <- many
      self$only <- only
      self$exclude <- self$opts$exclude %||% exclude
      self$ordered <- self$opts$ordered
      self$load_only <- load_only %||% self$opts$load_only
      self$dump_only <- dump_only %||% self$opts$dump_only
      self$partial <- partial
      self$unknown <- unknown %||% self$opts$unknown
      self$context <- context %||% list()
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
    #' @param many (logical) Should be set to `TRUE` if `obj` is a list of
    #' lists. default: `FALSE`
    #' @return list
    dump = function(x, many = FALSE) {
      x <- private$hndlr(x)
      parse_one <- function(z, self) {
        for (i in seq_along(z)) {
          if (!names(z)[i] %in% names(self$fields))
            stop("named element not in allowed set",
              call. = FALSE)
        }
        as.list(z)
      }
      if (!many) {
        parse_one(x, self)
      } else {
        lapply(x, parse_one, self = self)
      }
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
          # if (!inherits(raw_value, "Missing")) {
          val <- fld$deserialize(raw_value, key)
          # append to list
          ret[[ key ]] <- val
          # }
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

      if (!is.null(self$post_load) && is.function(self$post_load)) {
        ret <- if (many) lapply(ret, self$post_load) else self$post_load(ret)
        # make as_df=FALSE if custom object returned
        as_df <- FALSE
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

      self$load_json(jsonlite::toJSON(data), many = many, partial = partial,
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
