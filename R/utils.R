conds_text <- function(..., .sep = " & ") {
  conds <- rlang::enexprs(...)
  texts <- purrr::map_chr(conds, rlang::expr_text)
  paste(texts, collapse = .sep)
}

dots_negate <- function(...) {
  lapply(rlang::enquos(...), quo_negate)
}

quo_negate <- function(quo) {
  expr <- rlan::quo_get_expr(quo)
  rlang::quo_set_expr(quo, expr_negate(expr))
}

expr_negate <- function(x) {
  if (!is_logic_call(x)) {
    return(rlang::call2("!", x))
  }

  fn <- rlang::call_name(x)
  args <- rlang::call_args(x)

  # Just strip negations.
  if (fn == "!") {
    return(args[[1L]])
  }

  fn <- switch(fn,
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

  # Apply De Morgan's laws.
  if (fn %in% c("&", "|", "&&", "||")) {
    args <- lapply(args, expr_negate)
  }

  rlang::call2(fn, !!!args)
}

is_logic_call <- function(x) {
  rlang::is_call(x) && rlang::call_name(x) %in% .logic_ops
}

.logic_ops <- c("!", "&", "|", "&&", "||", "==", "!=", ">", ">=", "<", "<=")
