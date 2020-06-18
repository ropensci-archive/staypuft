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
#' fields$character()
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
