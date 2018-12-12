#' get value
#' 
#' @noRd
#' @param obj x
#' @param key x
#' @param default x
#' Helper for pulling a keyed value off various types of objects. Fields use
#' this method by default to access attributes of the source object. For object `x`
#' and attribute `i`, this method first tries to access `x[i]`, and then falls back to
#' `x.i` if an exception is raised.
#' 
#' .. warning::
#' If an object `x` does not raise an exception when `x[i]` does not exist,
#' `get_value` will never check the value `x.i`. Consider overriding
#' `marshmallow.fields.Field.get_value` in this case.
utils_get_value <- function(obj, key, default = miss_ing) {
  # if (not isinstance(key, int) and '.' in key) {
    # return get_value_for_keys(obj, key.split('.'), default)
  # } else {
    get_value_for_key(obj, key, default)
  # }
}

# get_value_for_keys <- function(obj, keys, default) {
#   if (length(keys) == 1) {
#     get_value_for_key(obj, keys[0], default)
#   } else {
#     get_value_for_keys(
#       get_value_for_key(obj, keys[0], default), 
#       keys[1:], 
#       default
#     )
#   }
# }

get_value_for_key <- function(obj, key, default) {
  # if not hasattr(obj, '__getitem__'):
  #     return getattr(obj, key, default)

  # try:
  #     return obj[key]
  # except (KeyError, IndexError, TypeError, AttributeError):
  #     return getattr(obj, key, default)
  return(obj[[key]])
}
