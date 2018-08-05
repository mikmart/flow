#' @export
filter.flow_df <- function(.data, ...) {
  included <- reconstruct(NextMethod(), .data)
  update_flow(included, .data, conds_text(...))
}

#' @export
arrange.flow_df <- function(.data, ...) {
  reconstruct(NextMethod(), .data)
}

#' @export
mutate.flow_df <- function(.data, ...) {
  reconstruct(NextMethod(), .data)
}
