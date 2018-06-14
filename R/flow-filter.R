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

expr_negate <- function(x) {
  if (!is_logic_call(x)) {
    return(rlang::lang("!", x))
  }

  fn <- as.character(x[[1L]])
  args <- x[-1L]

  if (fn == "!") {
    return(args[[1L]])
  }

  fn <- switch_logic_fn(fn)

  if (fn %in% c("&", "|", "&&", "||")) {
    # De Morgan: negate arguments, recursively
    args <- lapply(args, expr_negate)
  }

  rlang::lang(fn, !!!args)
}

is_logic_call <- function(x) {
  if (!is.call(x)) {
    return(FALSE)
  }

  fn <- as.character(x[[1L]])

  logic <- c("!", "&", "|", "&&", "||")
  comp <- c("==", "!=", ">", ">=", "<", "<=")

  fn %in% c(logic, comp)
}

switch_logic_fn <- function(x) {
  switch(x,
     "&" = "|",
     "|" = "&",
    "&&" = "||",
    "||" = "&&",
    "==" = "!=",
    "!=" = "==",
     ">" = "<=",
    ">=" = "<",
     "<" = ">=",
    "<=" = ">"
  )
}
