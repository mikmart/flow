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
  included <- dplyr::filter(as_tibble(.data), ...)
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
