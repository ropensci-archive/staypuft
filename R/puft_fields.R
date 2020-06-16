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
#' - `Any`
#' - more coming soon
#' 
#' @examples
#' puft_fields
#' puft_fields$field()
#' puft_fields$missing()
#' puft_fields$character()
#' puft_fields$number()
#' puft_fields$integer()
#' puft_fields$uuid()
#' puft_fields$boolean()
#' puft_fields$any()
#' puft_fields$date()
puft_fields <- list(
  field = function(...) Field$new(...),
  missing = function(...) Missing$new(...),
  character = function(...) Character$new(...),
  number = function(...) Number$new(...),
  integer = function(...) Integer$new(...),
  uuid = function(...) UUID$new(...),
  boolean = function(...) Boolean$new(...),
  any = function(...) Any$new(...),
  date = function(...) XDate$new(...)
)
