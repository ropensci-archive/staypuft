#' staypuft fields
#' 
#' @export
#' @name fields
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
#' fields
#' fields$field()
#' fields$missing()
#' fields$character()
#' fields$number()
#' fields$integer()
#' fields$uuid()
#' fields$boolean()
#' fields$any()
#' fields$date()
#' fields$nested()
fields <- list(
  field = function(...) Field$new(...),
  missing = function(...) Missing$new(...),
  character = function(...) Character$new(...),
  number = function(...) Number$new(...),
  integer = function(...) Integer$new(...),
  uuid = function(...) UUID$new(...),
  boolean = function(...) Boolean$new(...),
  any = function(...) Any$new(...),
  date = function(...) XDate$new(...),
  nested = function(...) Nested$new(...)
)
