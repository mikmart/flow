context("test-flow.R")

test_that("flow_tbl is not equal to tbl_df", {
  tbl <- tibble::tibble(x = c(1, 1, 2), y = 1:3)
  expect_failure(expect_equal(flow(tbl), tbl))
})

test_that("grouped_df is preserved", {
  tbl <- tibble::tibble(x = c(1, 1, 2), y = 1:3)
  expect_is(flow(dplyr::group_by(tbl, x)), "grouped_df")
})
