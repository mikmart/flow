flow <- function(x, initial = "Initial population") {
  flow_df <- tibble::tibble(
    step = initial,
    included = nrow(x),
    excluded = NA_integer_
  )
  new_flow(x, flow_df)
}

#' @export
track <- flow

new_flow <- function(x, flow, ..., subclass = NULL) {
  tibble::new_tibble(x, flow = flow, ..., subclass = c(subclass, "flow_df"))
}

reconstruct <- function(new, old) {
  UseMethod("reconstruct", old)
}

reconstruct.flow_df <- function(new, old) {
  new_flow(new, flow = get_flow(old))
}

get_flow <- function(x) {
  attr(x, "flow")
}

#' @export
chart <- get_flow

set_flow <- function(x, value) {
  attr(x, "flow") <- value
  x
}

add_step <- function(x, step, n_incl, n_excl) {
  old <- get_flow(x)
  new <- tibble::add_row(old,
    step = step,
    included = n_incl,
    excluded = n_excl
  )
  set_flow(x, new)
}

update_flow <- function(included, original, step) {
  n_incl <- nrow(included)
  n_excl <- nrow(original) - n_incl
  add_step(included, step, n_incl, n_excl)
}
