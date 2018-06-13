#' @export
#' @S3method filter flow_df
filter.flow_df <- function(.data, ...) {
  included <- reconstruct(NextMethod(), .data)
  update_flow(included, .data, conds_text(...))
}

conds_text <- function(..., .sep = " & ") {
  conds <- rlang::enexprs(...)
  conds <- purrr::map_chr(conds, rlang::expr_text)
  paste(conds, collapse = .sep)
}
