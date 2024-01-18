#' @export
include <- function(.data, ...) {
  UseMethod("include")
}

#' @export
include.data.frame <- function(.data, ...) {
  include(as_flow(.data), ...)
}

#' @importFrom rlang %||%
#' @export
include.flow_df <- function(.data, ..., .step = NULL) {
  included <- dplyr::filter(as_tibble(.data), ...)
  included <- reconstruct(included, .data)
  update_flow(included, .data, .step %||% step_auto_name(...))
}
