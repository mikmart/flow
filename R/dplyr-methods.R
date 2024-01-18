#' @exportS3Method dplyr::filter
filter.flow_df <- function(.data, ...) {
  included <- reconstruct(NextMethod(), .data)
  update_flow(included, .data, step_auto_name(...))
}

#' @exportS3Method dplyr::arrange
arrange.flow_df <- function(.data, ...) {
  reconstruct(NextMethod(), .data)
}

#' @exportS3Method dplyr::mutate
mutate.flow_df <- function(.data, ...) {
  reconstruct(NextMethod(), .data)
}
