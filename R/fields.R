#' staypuft fields
#'
#' @export
#' @name fields
#' @section Types of fields:
#'
#' - [Missing]
#' - [Character]
#' - [UUID]
#' - [Number]
#' - [Integer]
#' - [Boolean]
#' - [Any]
#' - [Date]
#' - [Nested]
#' - [Url]
#' - more coming soon
#'
#' @section Usage:
#' You can use any supported fields in the `fields` object.
#'
#' You can call the field like `fields$character()`,
#' or pass supported arguments like `fields$character(data_key = "foobar")`
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
  nested = function(...) Nested$new(...),
  url = function(...) Url$new(...)
)
