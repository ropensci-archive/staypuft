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
#' x <- Field$new()
#' x
#' x$error_messages
#' 
#' 
#' z <- Character$new()
#' z
#' z$error_messages
#' z$serialize(attr = "foo", obj = list(foo = "bar"))
#' z$deserialize("foo")
#' z$deserialize(miss_ing)
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

    initialize = function(
      default=miss_ing, attribute=NULL, data_key=NULL,
      validate=NULL, required=FALSE, allow_none=NULL, load_only=FALSE,
      dump_only=FALSE, missing=miss_ing, error_messages=NULL) {

      self$default = default
      self$attribute = attribute
      self$data_key = data_key
      self$validate = validate
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
          error_messages={self$error_messages %||% "none"}'), sep = "\n  ")
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
#' @name staypuft_fields
#' @details
#' types of fields: 
#' 
#' - `Missing`
#' - `Character`
#' - more coming ...
NULL

#' @export
#' @rdname staypuft_fields
Missing = R6::R6Class("Missing",
  public = list(
    class_name = "Missing",
    bool = function() return(FALSE),
    print = function(x, ...) {
      cat('<marshmallow.missing>')
    }
  )
)
#' @export
#' @rdname staypuft_fields
miss_ing = Missing$new()

#' @export
#' @rdname staypuft_fields
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

MISSING_ERROR_MESSAGE = 
  'ValidationError raised by `{self$class_name}`, but error key `{key}` does 
     not exist in the `error_messages` list'
