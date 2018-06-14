#' @export
filter.flow_df <- function(.data, ...) {
  included <- reconstruct(NextMethod(), .data)
  update_flow(included, .data, conds_text(...))
}

#' @export
group_by.flow_df <- function(.data, ..., add = FALSE) {
  as_flow(NextMethod(), flow_table = flow_table(.data))
}
