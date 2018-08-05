flow <- function(x, initial = "Initial population") {
  flow_table <- tibble::tibble(
    step = initial,
    included = nrow(x),
    excluded = NA_integer_
  )
  new_flow(x, flow_table = flow_table)
}

#' @export
track <- flow

new_flow <- function(x, flow_table, ..., subclass = NULL) {
  tibble::new_tibble(x,
    flow_table = flow_table,
    ...,
    subclass = c(subclass, "flow_df")
  )
}

unflow <- function(x) {
  structure(x, flow_table = NULL, class = setdiff(class(x), "flow_df"))
}

as_tibble.flow_df <- function(x, ...) {
  as_tibble(unflow(x), ...)
}

as.data.frame.flow_df <- function(x, ...) {
  as.data.frame(unflow(x), ...)
}

as_flow <- function(x, ...) {
  UseMethod("as_flow")
}

as_flow.flow_df <- function(x, ...) x

as_flow.data.frame <- function(x, ...) {
  flow(x)
}

reconstruct <- function(new, old) {
  UseMethod("reconstruct", old)
}

reconstruct.flow_df <- function(new, old) {
  new_flow(new, flow_table = flow_table(old))
}

flow_table <- function(x, ...) {
  UseMethod("flow_table")
}

flow_table.flow_df <- function(x, ...) {
  attr(x, "flow_table")
}

flow_table.data.frame <- function(x, ...) {
  NULL
}

#' @export
chart <- flow_table

set_flow_table <- function(x, value) {
  attr(x, "flow_table") <- value
  x
}

`flow_table<-` <- function(x, value) {
  set_flow_table(x, value)
}

add_step <- function(x, step, n_incl, n_excl) {
  old <- flow_table(x)
  new <- tibble::add_row(old,
    step = step,
    included = n_incl,
    excluded = n_excl
  )
  set_flow_table(x, new)
}

update_flow <- function(included, original, step) {
  n_incl <- nrow(included)
  n_excl <- nrow(original) - n_incl
  add_step(included, step, n_incl, n_excl)
}

all.equal.flow_df <- function(target, current, ...) {
  msg <- attr.all.equal(target, current)
  if (is.null(msg)) return(NextMethod())
  if (isTRUE(next_msg <- NextMethod()))
    msg else c(msg, next_msg)
}
