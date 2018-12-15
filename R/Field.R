#' Field
#' 
#' Basic field from which other fields should extend. It applies no
#' formatting by default, and should only be used in cases where
#' data does not need to be formatted before being serialized or deserialized.
#' On error, the name of the field will be returned.
#' 
#' @export
#' @param default If set, this value will be used during serialization if 
#' the input value is missing. If not set, the field will be excluded from
#' the serialized output if the input value is missing. May be a value or 
#' a callable.
#' @param attribute The name of the key to get the value from when 
#' deserializing. If `None`, assumes the key has the same name as the 
#' field.
#' @param data_key The name of the key to get the value from when 
#' deserializing. If `None`, assumes the key has the same name as the field.
#' @param validate Validator or collection of validators that 
#' are called during deserialization. Validator takes a field's input 
#' value as its only parameter and returns a boolean. If it returns `FALSE`, 
#' an :exc:`ValidationError` is raised.
#' @param required  Raise a :exc:`ValidationError` if the field value
#' is not supplied during deserialization.
#' @param allow_none  Set this to `TRUE` if `None` should be considered a 
#' valid value during validation/deserialization. If `missing=NULL` 
#' and `allow_none` is unset, will default to `TRUE`. Otherwise, the 
#' default is `FALSE`.
#' @param load_only If `TRUE` skip this field during serialization, 
#' otherwise its value will be present in the serialized data.
#' @param dump_only If `TRUE` skip this field during deserialization, 
#' otherwise its value will be present in the deserialized object. In the 
#' context of an HTTP API, this effectively marks the field as "read-only".
#' @param missing  Default deserialization value for the field if the field 
#' is not found in the input data. May be a value or a callable.
#' @param error_messages Overrides for `Field.default_error_messages`.
#' @param metadata  Extra arguments to be stored as metadata.
#' @details
#' **Methods**
#'   \describe{
#'     \item{`serialize(x, ...)`}{
#'      Pulls the value for the given key from the object, applies the
#'      field's formatting and returns the result.
#'      :param str attr: The attribute or key to get from the object.
#'      :param str obj: The object to pull the key from.
#'      :param callable accessor: Function used to pull values from `obj`.
#'      :param dict kwargs': Field-specific keyword arguments.
#'      :raise ValidationError: In case of formatting problem
#'     }
#'     \item{`deserialize(x, ...)`}{
#'      Deserialize `value`.
#'      :param value: The value to be deserialized.
#'      :param str attr: The attribute/key in `data` to be deserialized.
#'      :param dict data: The raw input data passed to the `Schema.load`.
#'      :param dict kwargs': Field-specific keyword arguments.
#'      :raise ValidationError: If an invalid value is passed or if a required value
#'          is missing.
#'     }
#'     \item{`serialize_(x, ...)`}{
#'      Serializes `value` to a basic Python datatype. Noop by default.
#'      Concrete :class:`Field` classes should implement this method.
#'      :param value: The value to be serialized.
#'      :param str attr: The attribute or key on the object to be serialized.
#'      :param object obj: The object the value was pulled from.
#'      :param dict kwargs': Field-specific keyword arguments.
#'      :raise ValidationError: In case of formatting or validation failure.
#'      :return: The serialized value
#'     }
#'     \item{`deserialize_(x, ...)`}{
#'      Deserialize value. Concrete :class:`Field` classes should implement this method.
#'      :param value: The value to be deserialized.
#'      :param str attr: The attribute/key in `data` to be deserialized.
#'      :param dict data: The raw input data passed to the `Schema.load`.
#'      :param dict kwargs': Field-specific keyword arguments.
#'      :raise ValidationError: In case of formatting or validation failure.
#'      :return: The deserialized value.
#'      .. versionchanged:: 3.0.0
#'          Add `**kwargs` parameters
#'      .. versionchanged:: 2.0.0
#'          Added `attr` and `data` parameters.
#'     }
#'     \item{`get_value(x, ...)`}{
#'      Return the value for a given key from an object.
#'      :param object obj: The object to get the value from
#'      :param str attr: The attribute/key in `obj` to get the value from.
#'      :param callable accessor: A callable used to retrieve the value of `attr` from
#'       the object `obj`. Defaults to `marshmallow.utils.get_value`.
#'     }
#'     \item{`validate_(x, ...)`}{
#'      Perform validation on `value`. Raise a :exc:`ValidationError` if validation
#'      does not succeed.
#'     }
#'     \item{`validate_missing_(x, ...)`}{
#'      Validate missing values. Raise a :exc:`ValidationError` if
#'      `value` should be considered missing.
#'     }
#'     \item{`fail(x, ...)`}{
#'      A helper method that simply raises a `ValidationError`.
#'     } 
#'   }
#'
#' @format NULL
#' @usage NULL
#' @examples
#' x <- puft_fields$field()
#' x
#' x$error_messages
#' 
#' z <- puft_fields$character()
#' z
#' z$error_messages
#' z$serialize(attr = "foo", obj = list(foo = "bar"))
#' z$deserialize("foo")
#' z$deserialize(puft_fields$missing())
Field <- R6::R6Class(
  "Field",
  public = list(
    class_name = "Field",

    # Some fields, such as Method fields and Function fields, are not expected
    #  to exist as attributes on the objects to serialize. Set this to FALSE
    #  for those fields
    CHECK_ATTRIBUTE = TRUE,

    # Used for sorting
    creation_index = 0,
    
    default = NULL,
    attribute = NULL,
    data_key = NULL,
    validate = NULL,
    required = NULL,
    allow_none = NULL,
    load_only = NULL,
    dump_only = NULL,
    missing = NULL,
    metadata = NULL,
    error_messages = list(),
    validators = list(),

    initialize = function(
      default=miss_ing, attribute=NULL, data_key=NULL,
      validate=NULL, required=FALSE, allow_none=NULL, load_only=FALSE,
      dump_only=FALSE, missing=miss_ing, error_messages=NULL) {

      self$default = default
      self$attribute = attribute
      self$data_key = data_key
      self$validate = validate
      self$validators = list()
      # if utils.is_iterable_but_not_string(validate):
      #     if not utils.is_generator(validate):
      #         self$validators = validate
      #     else:
      #         self$validators = list(validate)
      # elif callable(validate):
      #     self$validators = [validate]
      # elif validate is None:
      #     self$validators = []
      # else:
      #     raise ValueError(
      #         "The 'validate' parameter must be a callable "
      #         'or a collection of callables.',
      #     )

      self$required = required
      # If missing=NULL, None should be considered valid by default
      if (is.null(allow_none)) {
        if (is.null(missing)) {
          self$allow_none = TRUE
        } else {
          self$allow_none = FALSE
        }
      } else {
        self$allow_none = allow_none
      }
      self$load_only = load_only
      self$dump_only = dump_only
      self$missing = missing
      # self$metadata = metadata
      self$creation_index <- self$creation_index + 1

      # Collect default error message from self and parent classes
      #: Default error messages for various kinds of errors. The keys in this dictionary
      #: are passed to `Field.fail`. The values are error messages passed to
      err_messages = list(
        required = 'Missing data for required field.',
        null = 'Field may not be null.',
        validator_failed = 'Invalid value.'
      )
      self$error_messages = c(err_messages, self$error_messages_)
    },

    print = function(x, ...) {
      cat(glue::glue('<fields.{self$class_name}>'), sep = "\n  ")
      cat(glue::glue('default={self$default$class_name}
          attribute={self$attribute %||% "none"}
          validate={self$validate %||% "none"}
          required={self$required}
          load_only={self$load_only}
          dump_only={self$dump_only}
          missing={self$missing$class_name}
          allow_none={self$allow_none %||% "none"}
          error_messages={print_err_mssgs(self$error_messages %||% "none")}'), sep = "\n  ")
    },

    get_value = function(obj, attr, accessor=NULL, default=miss_ing) {
      # NOTE: Use getattr instead of direct attribute access here so that
      # subclasses aren't required to define `attribute` member
      attribute = self$attribute %||% NULL
      # accessor_func = accessor or utils_get_value
      accessor_func = utils_get_value
      check_key = if (is.null(attribute)) attr else attribute
      accessor_func(obj, check_key, default)
    },

    validate_ = function(value) {
      # errors = list()
      # kwargs = {}
      # for validator in self$validators:
      #     try:
      #         r = validator(value)
      #         if not isinstance(validator, Validator) and r is FALSE:
      #             self$fail('validator_failed')
      #     except ValidationError as err:
      #         kwargs.update(err.kwargs)
      #         if isinstance(err.messages, dict):
      #             errors.append(err.messages)
      #         else:
      #             errors.extend(err.messages)
      for (i in seq_along(self$validators)) {
        b <- tryCatch(self$validators[[i]](value), error = function(e) e)
        if (inherits(b, "error")) {
          stop(b$message)
        }
      }

      # if errors:
      #     raise ValidationError(errors, **kwargs)
    },

    fail = function(key) {
      msg <- self$error_messages[[key]]
      if (is.null(msg)) {
        msg <- glue::glue(MISSING_ERROR_MESSAGE)
        stop(msg)
      }
      stop("ValidationError: ", msg)
    },

    validate_missing_ = function(value) {
      if (inherits(value, "Missing")) {
        if (!is.null(self$required) && self$required) self$fail('required')
      }
      if (is.null(value)) {
        if (!is.null(self$allow_none) && !self$allow_none) self$fail('null') 
        # if hasattr(self, 'allow_none') and self$allow_none is not TRUE:
        #     self$fail('null')
      }
    },

    serialize = function(attr, obj, accessor=NULL) {
      if (self$CHECK_ATTRIBUTE) {
        value = self$get_value(obj, attr, accessor = accessor)
        if (inherits(value, "Missing") && !is.null(self$default)) {
          default = self$default
          value = default
          # value = default() if callable(default) else default
        }
        if (inherits(value, "Missing")) return(value)
      } else {
        value = NULL
      }
      self$serialize_(value, attr, obj)
    },

    deserialize = function(value, attr=NULL, data=NULL) {
      # Validate required fields, deserialize, then validate deserialized value
      self$validate_missing_(value)
      if (inherits(value, "Missing")) {
        miss = self$missing
        # return miss() if callable(miss) else miss
        if (is.function(miss)) miss() else miss
      }
      if (self$allow_none && is.null(value)) return(NULL)
      output = self$deserialize_(value, attr, data)
      self$validate_(output)
      return(output)
    },

    # # Methods for concrete classes to override.
    # def _bind_to_schema(self, field_name, schema):
    #     """Update field with values from its parent schema. Called by
    #         :meth:`_bind_field<marshmallow.Schema._bind_field>`.
    #     :param str field_name: Field name set in schema.
    #     :param Schema schema: Parent schema.
    #     """
    #     self$parent = self$parent or schema
    #     self$name = self$name or field_name

    serialize_ = function(value, attr = NULL, obj = NULL) return(value),
    deserialize_ = function(self, value, attr, data) return(value)

    # # Properties
    # @property
    # def context(self):
    #     """The context dictionary for the parent :class:`Schema`."""
    #     return self$parent.context

    # @property
    # def root(self):
    #     """Reference to the `Schema` that this field belongs to even if it is buried in a `List`.
    #     Return `None` for unbound fields.
    #     """
    #     ret = self
    #     while hasattr(ret, 'parent') and ret.parent:
    #         ret = ret.parent
    #     return ret if isinstance(ret, SchemaABC) else None
  )
)

#' staypuft fields
#' 
#' @export
#' @name puft_fields
#' @details
#' types of fields: 
#' 
#' - `Missing`
#' - `Character`
#' - `UUID`
#' - `Number`
#' - `Integer`
#' - `Boolean`
#' - more coming ...
#' @examples
#' puft_fields
#' puft_fields$field()
#' puft_fields$missing()
#' puft_fields$character()
#' puft_fields$number()
#' puft_fields$integer()
#' puft_fields$uuid()
#' puft_fields$boolean()
puft_fields <- list(
  field = function(...) {
    Field$new(...)
  },
  missing = function(...) {
    Missing$new(...)
  },
  character = function(...) {
    Character$new(...)
  },
  number = function(...) {
    Number$new(...)
  },
  integer = function(...) {
    Integer$new(...)
  },
  uuid = function(...) {
    UUID$new(...)
  },
  boolean = function(...) {
    Boolean$new(...)
  }
)

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


# missing error message
MISSING_ERROR_MESSAGE = 
  'ValidationError raised by `{self$class_name}`, but error key `{key}` does 
     not exist in the `error_messages` list'

print_err_mssgs <- function(x) {
  if (is.character(x)) return(x)
  tmp <- sprintf("%s: '%s'", names(x), unname(unlist(x)))
  paste(tmp, collapse = "; ")
}
