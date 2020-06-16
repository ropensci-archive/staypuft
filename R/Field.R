#' @title Field
#' @description Basic field from which other fields should extend
#' 
#' @details It applies no formatting by default, and should only be used
#' in cases where data does not need to be formatted before being
#' serialized or deserialized. On error, the name of the field will be
#' returned.
#' 
#' @export
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
  classname = "Field",
  inherit = FieldABC,
  public = list(
    #' @field class_name (character) xxx
    class_name = "Field",

    # Some fields, such as Method fields and Function fields, are not expected
    #  to exist as attributes on the objects to serialize. Set this to FALSE
    #  for those fields
    #' @field CHECK_ATTRIBUTE (logical) default: `TRUE`
    CHECK_ATTRIBUTE = TRUE,

    # Used for sorting
    #' @field creation_index (integer) xxx
    creation_index = 0,
    
    #' @field default a class, default: `Missing`
    default = NULL,
    #' @field attribute (character) xxx
    attribute = NULL,
    #' @field data_key (character) xxx
    data_key = NULL,
    #' @field validate xxx
    validate = NULL,
    #' @field required (logical) xxx
    required = NULL,
    #' @field allow_none (logical) xxx
    allow_none = NULL,
    #' @field load_only (logical) xxx
    load_only = NULL,
    #' @field dump_only (logical) xxx
    dump_only = NULL,
    #' @field missing (logical) xxx
    missing = NULL,
    #' @field metadata Extra arguments to be stored as metadata.
    metadata = NULL,
    #' @field error_messages (list) xxx
    error_messages = list(),
    #' @field validators (list) xxx
    validators = list(),

    #' @description Create a new Field object
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
    #' an `ValidationError` is raised.
    #' @param required  Raise a `ValidationError` if the field value
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
    #' @return A new `Field` object
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

    #' @description print method for Field objects
    #' @param x self
    #' @param ... ignored
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

    #' @description Return the value for a given key from an object.
    #' @param obj The object to get the value from
    #' @param attr The attribute/key in `obj` to get the value from.
    #' @param accessor (callback) A callable used to retrieve the value of `attr`
    #' @param default If set, this value will be used during serialization if 
    #' the input value is missing. If not set, the field will be excluded from
    #' the serialized output if the input value is missing. May be a value or 
    #' a callable.
    #' from the object `obj`. Defaults to `marshmallow.utils.get_value`.
    get_value = function(obj, attr, accessor=NULL, default=miss_ing) {
      # NOTE: Use getattr instead of direct attribute access here so that
      # subclasses aren't required to define `attribute` member
      attribute = self$attribute %||% NULL
      # accessor_func = accessor or utils_get_value
      accessor_func = utils_get_value
      check_key = if (is.null(attribute)) attr else attribute
      accessor_func(obj, check_key, default)
    },

    #' @description Perform validation on `value`. Raise a `ValidationError`
    #' if validation does not succeed.
    #' @param value a value
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
          stop(b$message, call.=FALSE)
        }
      }

      # if errors:
      #     raise ValidationError(errors, **kwargs)
    },

    #' @description A helper method that simply raises a
    #' `ValidationError`
    #' @param key a key
    fail = function(key) {
      msg <- self$error_messages[[key]]
      if (is.null(msg)) {
        msg <- glue::glue(MISSING_ERROR_MESSAGE)
        stop(msg)
      }
      stop("ValidationError: ", msg, call.=FALSE)
    },

    #' @description Validate missing values. Raise a `ValidationError`
    #' if `value` should be considered missing.
    #' @param value a value
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

    #' @description Pulls the value for the given key from the object,
    #' applies the field's formatting and returns the result.
    #' @param attr (character) The attribute or key to get from the object.
    #' @param obj (character) The object to pull the key from.
    #' @param accessor (callback) Function used to pull values from `obj`.
    #' @details raise ValidationError: In case of formatting problem
    #' @return xxxx
    # kwargs: Field-specific keyword arguments.
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

    #' @description Deserialize `value`.
    #' @param value The value to be deserialized.
    #' @param attr (character) The attribute/key in `data` to be deserialized.
    #' @param data (list) The raw input data passed to the `Schema.load`.
    #' @details raise ValidationError: If an invalid value is passed or if a
    #' required value is missing.
    # kwargs (list) Field-specific keyword arguments.
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

    # Methods for concrete classes to override.
    
    #' @description Update field with values from its parent schema.
    #' @param field_name (character) Field name set in schema.
    #' @param schema Parent schema.
    bind_to_schema = function(field_name, schema) {
      self$parent = self$parent %||% schema
      self$name = self$name %||% field_name
    },

    #' @description Serializes `value` to a basic Python datatype. Noop by
    #' default. Concrete :class:`Field` classes should implement this method.
    #' @param value The value to be deserialized.
    #' @param attr (character) The attribute/key in `data` to be deserialized.
    #' @param obj (character) The object to pull the key from.
    #' @details raise ValidationError: In case of formatting or validation
    #' failure.
    #' @return The serialized value
    # kwargs': Field-specific keyword arguments.
    serialize_ = function(value, attr = NULL, obj = NULL) return(value),

    #' @description Deserialize value. Concrete :class:`Field` classes should implement this method.
    #' @param value The value to be deserialized.
    #' @param attr (character) The attribute/key in `data` to be deserialized.
    #' @param data (list) The raw input data passed to the `Schema.load`.
    #' @details raise ValidationError: In case of formatting or validation failure.
    #' @return The deserialized value
    # kwargs: Field-specific keyword arguments.
    deserialize_ = function(value, attr, data) return(value),

    # Properties
    #' @description The context dictionary for the parent `Schema`
    context = function() {
      self$parent$context
    },

    #' @description Reference to the `Schema` that this field belongs
    #' to even if it is buried in a `List`
    #' @return `None` for unbound fields
    root = function() {
      ret <- self
      while (!is.null(ret$parent)) {
        ret <- ret$parent
      }
      if (inherits(ret, 'SchemaABC')) ret else NULL
    }
  )
)

# missing error message
MISSING_ERROR_MESSAGE = 
  'ValidationError raised by `{self$class_name}`, but error key `{key}` does 
     not exist in the `error_messages` list'

print_err_mssgs <- function(x) {
  if (is.character(x)) return(x)
  tmp <- sprintf("%s: '%s'", names(x), unname(unlist(x)))
  paste(tmp, collapse = "; ")
}
