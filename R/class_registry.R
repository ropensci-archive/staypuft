#' @title ClassRegistry
#' @description Base schema class with which to define custom schemas
#'
#' @export
#' @examples
#' x <- ClassRegistry$new()
#' x
#' x$registry
#' z <- Schema$new("FooBar",
#'   name = fields$character(),
#'   title = fields$character()
#' )
#' w <- Schema$new("MySchema",
#'   name = fields$character(),
#'   title = fields$character()
#' )
#' x
#' x$registry
#' x$register("FooBar", z)
#' x$register("MySchema", w)
#' x
#' x$registry
#' 
#' x$get_class("FooBar")
#' x$get_class("MySchema")
ClassRegistry <- R6::R6Class("ClassRegistry",
  public = list(
    #' @field registry (list) list of classes registered
    registry = list(),
    print = function(x) {
      cat("class registry", sep = "\n")
      cat(paste0("  registry (n): ", length(self$registry)), sep = "\n")
      reg <- self$registry[1:min(c(10, length(self$registry)))]
      cat(paste0("  names (first 10): ", paste0(names(reg), collapse=",")),
        sep = "\n")
    },
    #' @description Add a class to the registry of serializer classes
    #' @param class_name (character) the class name
    #' @param cls (Schema) the class object
    #' @return nothing, registers internally
    register = function(class_name, cls) {
      self$registry <- modifyList(self$registry,
        stats::setNames(list(cls), class_name))
    },
    get_class = function(class_name) {
      self$registry[[class_name]]
    }
  )
)

class_registry <- ClassRegistry$new()
