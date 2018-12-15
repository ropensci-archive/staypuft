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

Number <- R6::R6Class("Number",
  inherit = Field,
  public = list(
    class_name = "Number",
    as_string = FALSE,
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

Integer <- R6::R6Class("Integer",
  inherit = Number,
  public = list(
    class_name = "Integer",
    strict = FALSE,
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

# Url <- R6::R6Class("Url",
#   inherit = Character,
#   public = list(
#     class_name = "Url",
#     relative = NULL,
#     require_tld = NULL,
#     error_messages_ = list(
#       invalid_uuid = 'Not a valid URL.'
#     ),
#     initialize = function(..., relative = FALSE, schemes = NULL, 
#       require_tld = TRUE) {

#       super$initialize(...)
#       self$relative = relative
#       self$require_tld = require_tld
#       super$validators <- c(
#         super$validators,
#         validate$URL
#       )
#     },
#     validated = function(value) {
#       if (is.null(value)) return(NULL)
#       validate_url()
#     }
#   )
# )

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
