#' @title Missing
#' @description Missing class
#' @export
#' @keywords internal
Missing = R6::R6Class("Missing",
  public = list(
    class_name = "Missing",
    bool = function() return(FALSE),
    print = function(x, ...) {
      cat('<marshmallow.missing>')
    }
  )
)
miss_ing = Missing$new()

#' @title Character 
#' @description A string field
#' @export
#' @keywords internal
Character <- R6::R6Class("Character",
  inherit = Field,
  public = list(
    class_name = "Character",
    error_messages_ = list(
      invalid = 'Not a valid string.',
      invalid_utf8 = 'Not a valid utf-8 string.'
    ),
    serialize_ = function(value, attr = NULL, obj = NULL) {
      if (is.null(value)) return(NULL)
      return(as.character(value))
    },
    deserialize_ = function(value, attr = NULL, data = NULL) {
      # FIXME: see python class, more to do here maybe?
      if (inherits(value, "Missing")) value else as.character(value)
    }
  )
)

#' @title Date 
#' @description A formatted date string
#' @export
#' @keywords internal
#' @note e.g., value: 2014-12-22T03:12:58.019077+00:00
XDate <- R6::R6Class("Date",
  inherit = Field,
  public = list(
    class_name = "Date",
    format = NULL,
    default_format = "iso",
    obj_type = "datetime",
    #' @description Create a new Date object
    #' @param @param format Either "rfc" (for RFC822) or "iso" (for ISO8601)
    initialize = function(format = NULL) {
      super$initialize()
      self$format <- format
    },
    error_messages_ = list(
      invalid = 'Not a valid date.',
      invalid_awareness = 'Not a valid {awareness} {obj_type}.',
      format = '"{input}" cannot be formatted as a {obj_type}.'
    ),
    format_date = function(value) {
      if (!is.numeric(value)) {
        stop("value must be numeric")
      }
      as.numeric(value)
    },
    to_string = function(value) {
      as.character(value)
    },
    serialize_ = function(value, attr = NULL, obj = NULL) {
      if (is.null(value)) return(NULL)
      # data_format <- self$format %||% self$default_format
      parsedate::parse_date(value)
    },
    validated = function(value) {
      x <- parsedate::parse_date(value)
      if (is.na(x)) super$fail("invalid")
      return(x)
    },
    deserialize_ = function(value, attr = NULL, data = NULL) {
      self$validated(value)
    }
  )
)

#' @title UUID 
#' @description A UUID field
#' @export
#' @keywords internal
UUID <- R6::R6Class("UUID",
  inherit = Character,
  public = list(
    class_name = "UUID",
    error_messages_ = list(
      invalid_uuid = 'Not a valid UUID.'
    ),
    validated = function(value) {
      if (is.null(value)) return(NULL)
      # from https://stackoverflow.com/a/13653180/1091766
      uuid_regex <- "^[0-9a-f]{8}-[0-9a-f]{4}-[1-5][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$"
      if (!grepl(uuid_regex, value)) super$fail("invalid_uuid")
      return(value)
    },
    serialize_ = function(value, attr = NULL, obj = NULL) {
      if (is.null(value)) return(NULL)
      ret <- as.character(self$validated(value))
      super$serialize_(value)
    },
    deserialize_ = function(value, attr = NULL, data = NULL) {
      self$validated(value)
    }
  )
)

#' @title Number 
#' @description A Number field
#' @export
#' @keywords internal
Number <- R6::R6Class("Number",
  inherit = Field,
  public = list(
    class_name = "Number",
    as_string = FALSE,
    #' @description Create a new Integer object
    #' @param as_string If `TRUE`, serialize to a string instead of
    #' a `numeric` type
    initialize = function(..., as_string = FALSE) {
      super$initialize(...)
      self$as_string <- as_string
    },
    error_messages_ = list(
      invalid = 'Not a valid number.'
    ),
    format_num = function(value) {
      if (!is.numeric(value)) {
        stop("value must be numeric")
      }
      as.numeric(value)
    },
    validated = function(value) {
      if (is.null(value)) return(NULL)
      tmp <- tryCatch(self$format_num(value), error = function(e) e)
      if (inherits(tmp, "error")) super$fail("invalid")
      return(tmp)
    },
    to_string = function(value) {
      as.character(value)
    },
    serialize_ = function(value, attr = NULL, obj = NULL) {
      ret <- self$validated(value)
      if (self$as_string && !is.null(ret) && !inherits(ret, "Missing")) {
        self$to_string(ret)
      } else {
        ret
      }
    },
    deserialize_ = function(value, attr = NULL, data = NULL) {
      self$validated(value)
    }
  )
)

#' @title Integer 
#' @description A Integer field
#' @export
#' @keywords internal
Integer <- R6::R6Class("Integer",
  inherit = Number,
  public = list(
    class_name = "Integer",
    strict = FALSE,
    #' @description Create a new Integer object
    #' @param strict If `TRUE`, only integer types are valid. Otherwise,
    #' any value castable to `integer` is valid
    initialize = function(..., strict = FALSE) {
      super$initialize(...)
      self$strict <- strict
    },
    error_messages_ = list(
      invalid = 'Not a valid integer.'
    ),
    format_num = function(value) {
      if (self$strict) {
        if (is.integer(value)) return(super$format_num(value))
        super$fail("invalid")
      }
      super$format_num(value)
    }
  )
)

#' @title Boolean 
#' @description A boolean field
#' @export
#' @keywords internal
Boolean <- R6::R6Class("Boolean",
  inherit = Field,
  public = list(
    class_name = "Boolean",
    # Default truthy values
    truthy = c(
      't', 'T',
      'true', 'True', 'TRUE',
      'on', 'On', 'ON',
      '1', 1,
      TRUE
    ),
    # Default falsy values
    falsy = c(
      'f', 'F',
      'false', 'False', 'FALSE',
      'off', 'Off', 'OFF',
      '0', 0, 0.0,
      FALSE
    ),
    #' @description Create a new Boolean object
    #' @param truthy Values that will (de)serialize to `TRUE`. If an
    #' empty set, any non-falsy value will deserialize to `TRUE`. If `NULL`,
    #' xx will be used.
    #' @param falsy Values that will (de)serialize to `FALSE`. If `NULL`,
    #' xx will be used.
    initialize = function(..., truthy = NULL, falsy = NULL) {
      super$initialize(...)
      if (!is.null(truthy)) self$truthy <- c(self$truthy, truthy)
      if (!is.null(falsy)) self$falsy <- c(self$falsy, falsy)
    },
    error_messages_ = list(
      invalid = 'Not a valid boolean.'
    ),
    serialize_ = function(value, attr = NULL, obj = NULL) {
      if (is.null(value)) return(NULL)
      if (value %in% self$truthy) return(TRUE)
      if (value %in% self$falsy) return(FALSE)
      return(as.logical(value))
    },
    deserialize_ = function(value, attr = NULL, data = NULL) {
      if (length(self$truthy) == 0 || 
        all(is.na(self$truthy)) || 
        is.null(self$truthy)
      ) {
        return(as.logical(value))
      } else {
        if (value %in% self$truthy) return(TRUE)
        if (value %in% self$falsy) return(FALSE)
      }
      super$fail("invalid")
    }
  )
)

#' @title Url
#' @description A validated URL field. Validation occurs during both
#' serialization and deserialization
#' @export
#' @keywords internal
#' @examples
#' aschema <- Schema$new("aSchema",
#'   url = fields$url()
#' )
#' aschema
#' aschema$load(list(url = "https://ropensci.org/")) # good
#' if (interactive()) aschema$load(list(url = 6)) # bad
#' 
#' sch <- Schema$new("anotherschema",
#'   url = fields$url(schemes = c("https", "ftps"))
#' )
#' if (interactive()) sch$load(list(url = "http://google.com")) # bad
Url <- R6::R6Class("Url",
  inherit = Field,
  public = list(
    class_name = "Url",
    relative = NULL,
    require_tld = NULL,
    error_messages_ = list(
      invalid_url = 'Not a valid URL.'
    ),
    #' @description Create a new Url object
    #' @param relative Whether to allow relative URLs. NOT WORKING YET
    #' @param require_tld Whether to reject non-FQDN hostnames. NOT WORKING YET
    #' @param schemes Valid schemes. By default `http`, `https`, `ftp`, and
    #' `ftps` are allowed
    initialize = function(..., relative = FALSE, schemes = NULL, 
      require_tld = TRUE) {

      super$initialize(...)
      self$validators <- c(
        self$validators,
        self$validate_url(relative, schemes, require_tld)
      )
      self$relative = relative
      self$require_tld = require_tld
    },

    validate_url = function(relative = FALSE, schemes = NULL, require_tld = TRUE) {
      schemes_default <- c("http", "https", "ftp", "ftps")
      if (is.null(schemes)) schemes <- schemes_default
      scheme_chk <- schemes %in% schemes_default
      if (!all(scheme_chk)) 
        stop("1 or more schemes not in allowed set: ",
          paste0(schemes[!scheme_chk], collapse=", "))

      function(x) {
        valid_chars <- rex::rex(except_some_of(".", "/", " ", "-"))
        re <- rex::rex(
          start,
          # protocol identifier (optional) + //
          group(or(schemes), "://"),
          # user:pass authentication (optional)
          maybe(non_spaces,
            maybe(":", zero_or_more(non_space)),
            "@"),
          #host name
          group(zero_or_more(valid_chars, zero_or_more("-")), one_or_more(valid_chars)),
          #domain name
          zero_or_more(".", zero_or_more(valid_chars, zero_or_more("-")), one_or_more(valid_chars)),
          #TLD identifier
          group(".", at_least(valid_chars, 2)),
          # server port number (optional)
          maybe(":", between(digit, 2, 5)),
          # resource path (optional)
          maybe("/", zero_or_more(non_space)),
          end
        )
        # if (!grepl(re, x)) stop("bad URL")
        if (!grepl(re, x)) super$fail("invalid_url")
      }
    }
  )
)

#' @title Email
#' @description A validated email field. Validation occurs during both
#' serialization and deserialization
#' @export
#' @keywords internal
#' @examples
#' z <- Schema$new("email", email = fields$email())
#' z
#' z$load(list(email = "blueberries@yahoo.com")) # good
#' if (interactive()) z$load(list(email = 'foobar')) # bad
Email <- R6::R6Class("Email",
  inherit = Field,
  public = list(
    class_name = "Email",
    error_messages_ = list(
      invalid_email = 'Not a valid email address.'
    ),
    #' @description Create a new Email object
    initialize = function(...) {
      super$initialize(...)
      self$validators <- c(
        self$validators,
        self$validate_email()
      )
    },
    validate_email = function() {
      function(x) {
        chek_for_pkg("addressable")
        if (!addressable::Address$new(x)$valid()) super$fail("invalid_email")
      }
    }
  )
)

#' @title Any 
#' @description A field that applies no formatting
#' @export
#' @keywords internal
Any <- R6::R6Class("Any",
  inherit = Field,
  public = list(
    class_name = "Any",
    serialize_ = function(value, attr = NULL, obj = NULL) {
      return(value)
    },
    deserialize_ = function(value, attr = NULL, data = NULL) {
      return(value)
    }
  )
)

#' @title NamedList 
#' @description A class for lists with key-value pairs - aka in R: named lists
#' @export
#' @keywords internal
#' @examples
#' x <- Schema$new("foo",
#'   title = fields$character(),
#'   age = fields$integer(strict = TRUE),
#'   name = fields$named_list(keys=fields$character(), values=fields$number())
#' )
#' x
#' # good
#' x$load(list(
#'   title = "barry", 
#'   age = 3L, 
#'   name = list(foo = 3.4, iff = 5)))
#' # bad
#' if (interactive()) {
#' x$load(list(
#'   title = "barry", 
#'   age = 3L, 
#'   name = list(foo = 3.4, iff = "a")))
#' 
#' x$load(list(
#'   title = "barry", 
#'   age = 3L, 
#'   name = list("bar", "else")))
#' }
NamedList <- R6::R6Class("NamedList",
  inherit = Field,
  public = list(
    class_name = "NamedList",
    key_field = NULL,
    value_field = NULL,
    initialize = function(keys, values, ...) {
      super$initialize(...)
      if (!is.null(keys)) self$key_field <- keys
      if (!is.null(values)) self$value_field <- values
    },
    serialize_ = function(value, attr = NULL, obj = NULL) {
      if (is.null(value)) return(NULL)
      return("FIXME, not done yet")
    },
    deserialize_ = function(value, attr = NULL, data = NULL) {
      # Deserialize keys
      if (is.null(self$key_field)) {
        keys <- as.list(stats::setNames(names(value), names(value)))
      } else {
        keys <- list()
        for (i in seq_along(value)) {
          keys[names(value)[i]] <- self$key_field$deserialize(names(value)[i])
        }
      }

      # Deserialize values
      result <- list()
      if (is.null(self$value_field)) {
        for (i in seq_along(value)) {
          if (names(value)[i] %in% names(keys)) {
            result[ keys[[names(value)[i]]] ] <- value[[i]]
          }
        }
      } else {
        for (i in seq_along(value)) {
          z <- self$value_field$deserialize(value[[i]])
          if (names(value)[i] %in% names(keys)) {
            result[ keys[[names(value)[i]]] ] <- z
          }
        }
      }

      return(result)
    }
  )
)

#' @title Nested 
#' @description  Nest a Schema inside a field
#' @export
#' @keywords internal
#' @examples
#' artist_schema <- Schema$new("ArtistSchema",
#'   name = fields$character()
#' )
#' x <- Nested$new(artist_schema)
#' x
#' x$nested
#' x$deserialize_(value = list(name = 6)) # good
#' if (interactive()) x$deserialize_(value = list(foobar = 6)) # bad
Nested <- R6::R6Class("Nested",
  inherit = Field,
  public = list(
    class_name = "Nested",
    nested = NULL,
    only = NULL,
    exclude = NULL,
    many = FALSE,
    unknown = "raise",
    schema_ = NULL,
    #' @description Create a new Nested object
    #' @param nested The Schema class or class name (character)
    #' to nest, or "self" to nest the `Schema` within itself
    #' @param exclude A list or tuple of fields to exclude
    #' @param only A list or tuple of fields to marshal. If `NULL`, all fields
    #' are marshalled. This parameter takes precedence over `exclude`.
    #' @param many Whether the field is a collection of objects.
    #' @param unknown Whether to exclude, include, or raise an
    #' error for unknown fields in the data. Use "raise", "exclude",
    #' or "include"
    initialize = function(nested, default = miss_ing, exclude = NULL, 
      only = NULL, many = FALSE, unknown = "raise", ...) {

      super$initialize(...)
      if (!is.null(only) && something) stop("'only' should be a vector of strings")
      if (!is.null(exclude) && something) stop("'exclude' should be a vector of strings")
      self$nested = nested
      self$only = only
      self$exclude = exclude
      self$many = many
      self$unknown = unknown
    },
    error_messages_ = list(
      type = 'Invalid type.'
    ),
    schema = function() {
      if (is.null(self$schema_)) {
        # inherit context from parent
        context <- self$parent$context %||% list()
        if (inherits(self$nested, "SchemaABC")) {
          self$schema_ <- self$nested
          self$schema_$context <- c(self$schema_$context, context)
        } else {
          if (
            # inherits(self$nested, type) &&
            inherits(self$nested, "SchemaABC")
          ) {
            schema_class <- self$nested
          } else if (!inherits(self$nested, "character")) {
            stop("Nested fields must be passed a Schema")
          # } else if (self$nested == "self") {
          } else {
            schema_class = class_registry$get_class(self$nested$schema_name)
          }
          self$schema_ <- schema_class(
            many=self$many,
            only=self$only,
            exclude=self$exclude,
            context=context
            # load_only=self$_nested_normalized_option("load_only"),
            # dump_only=self$_nested_normalized_option("dump_only")
          )
        }
      }
      return(self$schema_)
    },
    serialize_ = function(value, nested_obj, attr = NULL, obj = NULL) {
      schema <- self$schema()
      if (is.null(nested_obj)) return(NULL)
      many <- schema$many %||% self$many
      return(schema$dump(nested_obj, many=many))
    },
    test_list = function(self, value) {
      many <- self$schema$many %||% self$many
      if (many && !is.list(value)) super$fail("type")
    },
    load_ = function(value, data = NULL, partial = FALSE) {
      self$schema()
      valid_data <- tryCatch(
        self$schema_$load(value, unknown=self$unknown, partial=partial),
        error = function(e) e)
      if (inherits(valid_data, "error")) stop(valid_data$message)
      return(valid_data)
    },
    deserialize_ = function(value, attr = NULL, data = NULL, partial = FALSE) {
      # self$test_list(value)
      return(self$load_(value, data, partial=partial))
    }
  )
)

# rex hack for R CMD CHECK
rex::register_shortcuts("staypuft")
