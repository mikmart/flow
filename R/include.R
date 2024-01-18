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

conds_text <- function(..., .sep = " & ") {
  conds <- rlang::enexprs(...)
  texts <- purrr::map_chr(conds, rlang::expr_text)
  paste(texts, collapse = .sep)
}
