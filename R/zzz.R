compact <- function(l) Filter(Negate(is.null), l)
strextract <- function(str, pattern) regmatches(str, regexpr(pattern, str))
strtrim <- function(str) gsub("^\\s+|\\s+$", "", str)
`%||%` <- function(x, y) if (is.null(x)) y else x
assert <- function (x, y) {
  if (!is.null(x)) {
    if (!inherits(x, y)) {
      stop(deparse(substitute(x)), " must be of class ",
          paste0(y, collapse = ", "), call. = FALSE)
    }
  }
}
must_include <- function(x, y) {
  if (!x %in% y) {
    stop(glue::glue("{x} must be one of {paste0(y, collapse = ', ')}"))
  }
}
chek_for_pkg <- function(x) {
  if (!requireNamespace(x, quietly = TRUE)) {
    stop("Please install ", x, call. = FALSE)
  } else {
    invisible(TRUE)
  }
}
# in a list, do all slots have names?
# has_names(x = list())
# has_names(x = list(a = 5, b = 6))
# has_names(x = list(a = 5, 6))
has_names <- function(x) {
  stopifnot(is.list(x))
  if (length(x) == 0) return(TRUE)
  length(Filter(nzchar, names(x))) == length(x)
}
