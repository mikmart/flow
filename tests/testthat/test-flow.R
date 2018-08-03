context("test-flow.R")

test_that("flow_tbl is not equal to tbl_df", {
  tbl <- tibble::tibble(x = c(1, 1, 2), y = 1:3)
  expect_failure(expect_equal(flow(tbl), tbl))
})
