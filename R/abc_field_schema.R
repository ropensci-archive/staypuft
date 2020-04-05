#' @title FieldABC
#' @description Abstract base class from which all Field classes inherit
#' @export
#' @keywords internal
#' @examples \dontrun{
#' x <- FieldABC$new()
#' x
#' # x$serialize()
#' # x$deserialize()
#' # x$serialize_()
#' # x$deserialize_()
#' }
FieldABC <- R6::R6Class("FieldABC",
  public = list(
    #' @field parent xxx
    parent = NULL,
    #' @field name xxx
    name = NULL,

    #' @description serialize fun: to be replaced by child class
    serialize = function() stop('Not Implemented'),
    #' @description deserialize fun: to be replaced by child class
    deserialize = function() stop('Not Implemented'),
    #' @description serialize_ fun: to be replaced by child class
    serialize_ = function() stop('Not Implemented'),
    #' @description deserialize_ fun: to be replaced by child class
    deserialize_ = function() stop('Not Implemented')
    # __deepcopy__ = function(memo) {
    #   ret = copy.copy(self)
    #   return(ret)
    # }
  )
)

#' @title SchemaABC
#' @description Abstract base class from which all Schemas inherit
#' @export
#' @keywords internal
#' @examples \dontrun{
#' x <- SchemaABC$new()
#' x
#' # x$dump()
#' # x$dump_json()
#' # x$load()
#' # x$load_json()
#' }
SchemaABC <- R6::R6Class("SchemaABC",
  public = list(
    #' @description dump fun: to be replaced by child class
    dump = function() stop('Not Implemented'),
    #' @description dump_json fun: to be replaced by child class
    dump_json = function() stop('Not Implemented'),
    #' @description load fun: to be replaced by child class
    load = function() stop('Not Implemented'),
    #' @description load_json fun: to be replaced by child class
    load_json = function() stop('Not Implemented')
  )
)
