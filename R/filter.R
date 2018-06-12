#' @export
#' @S3method filter flow_df
filter.flow_df <- function(.data, ...) {
  included <- reconstruct(NextMethod(), .data)

  conds <- conds_text(...)
  n_incl <- nrow(included)
  n_excl <- nrow(.data) - n_incl

  add_step(included, conds, n_incl, n_excl)
}

conds_text <- function(..., .sep = " & ") {
  conds <- rlang::enexprs(...)
  conds <- purrr::map_chr(conds, rlang::expr_text)
  paste(conds, collapse = .sep)
}
