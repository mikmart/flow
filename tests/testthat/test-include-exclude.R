context("test-include-exclude.R")

test_that("exclude does negated include", {
  df <- data.frame(x = 1:5, y = 5:1)

  expect_equal(exclude(df, x == y), include(df, x != y))
  expect_equal(exclude(df, x != y), include(df, x == y))

  expect_equal(exclude(df, x > y), include(df, x <= y))
  expect_equal(exclude(df, x >= y), include(df, x < y))

  expect_equal(exclude(df, x < y), include(df, x >= y))
  expect_equal(exclude(df, x <= y), include(df, x > y))

  expect_equal(exclude(df, x > 1, y <= 3), include(df, x <= 1, y > 3))
})
