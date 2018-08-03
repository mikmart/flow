conds_text <- function(..., .sep = " & ") {
  conds <- rlang::enexprs(...)
  texts <- purrr::map_chr(conds, rlang::expr_text)
  paste(texts, collapse = .sep)
}

conds_negate <- function(...) {
  conds <- rlang::enquos(...)
  exprs <- purrr::map(conds, rlang::quo_get_expr)
  exprs <- purrr::map(exprs, expr_negate)
  purrr::map2(conds, exprs, rlang::quo_set_expr)
}

expr_negate <- function(x) {
  if (!is_logic_call(x)) {
    return(rlang::lang("!", x))
  }

  fn <- rlang::call_name(x)
  args <- rlang::call_args(x)

  if (fn == "!") {
    # Unary operator, so only ever 1 arg
    return(args[[1L]])
  }

  fn <- negate_logic(fn)

  if (fn %in% c("&", "|", "&&", "||")) {
    # De Morgan: negate arguments, recursively
    args <- lapply(args, expr_negate)
  }

  rlang::lang(fn, !!!args)
}

is_logic_call <- function(x) {
  if (!rlang::is_call(x)) {
    return(FALSE)
  }

  rlang::call_name(x) %in% .logic_ops
}

.logic_ops <- c("==", "!=", ">", ">=", "<", "<=",
                "!", "&", "|", "&&", "||")

negate_logic <- function(x) {
  switch(x,
     "&" = "|",
     "|" = "&",
    "&&" = "||",
    "||" = "&&",
    "==" = "!=",
    "!=" = "==",
     ">" = "<=",
    "<=" = ">",
    ">=" = "<",
     "<" = ">="
  )
}
