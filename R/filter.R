#' @export
filter.flow_df <- function(.data, ...) {
  included <- reconstruct(NextMethod(), .data)
  update_flow(included, .data, conds_text(...))
}

#' @export
include <- function(.data, ...) {
  UseMethod("include")
}

#' @export
include.data.frame <- function(.data, ...) {
  include(as_flow(.data), ...)
}

#' @importFrom purrr %||%
#' @export
include.flow_df <- function(.data, ..., .step = NULL) {
  included <- dplyr::filter(unflow(.data), ...)
  included <- reconstruct(included, .data)

  step <- .step %||% conds_text(...)
  update_flow(included, .data, step)
}

#' @export
exclude <- function(.data, ...) {
  UseMethod("exclude")
}

#' @export
exclude.data.frame <- function(.data, ...) {
  exclude(as_flow(.data), ...)
}

#' @export
exclude.flow_df <- function(.data, ..., .step = NULL) {
  include(.data, !!!conds_negate(...), .step = .step)
}


# Helpers -----------------------------------------------------------------

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

conds_text <- function(..., .sep = " & ") {
  conds <- rlang::enexprs(...)
  texts <- exprs_text(conds)
  paste(texts, collapse = .sep)
}

exprs_text <- function(exprs, ...) {
  purrr::map_chr(exprs, rlang::expr_text, ...)
}

conds_negate <- function(...) {
  conds <- rlang::enexprs(...)
  purrr::map(conds, expr_negate)
}

expr_negate <- function(expr) {
  rlang::expr(!(!!expr))
}
