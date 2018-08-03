context("test-dplyr-methods.R")

test_that("dplyr functions preserve flow_df", {
  df <- flow(data.frame(x = c(1, 2, 2), y = 6:4))
  grouped <- dplyr::group_by(df, x)

  expect_is(dplyr::select(df, x), "flow_df")
  expect_is(dplyr::filter(df, x > 1), "flow_df")
  expect_is(dplyr::mutate(df, z = 7), "flow_df")
  expect_is(dplyr::rename(df, foo = x), "flow_df")
  expect_is(dplyr::arrange(df, y), "flow_df")
  expect_is(dplyr::group_by(df, x), "flow_df")
  expect_is(dplyr::transmute(df, z = 7), "flow_df")

  expect_is(dplyr::filter(grouped, mean(y) < 6), "flow_df")
  expect_is(dplyr::mutate(grouped, z = mean(y)), "flow_df")
  expect_is(dplyr::add_count(grouped, x), "flow_df")

  expect_failure({
    expect_is(dplyr::count(df, x), "flow_df")
    expect_is(dplyr::tally(grouped), "flow_df")
    expect_is(dplyr::summarize(grouped, z = mean(y)), "flow_df")
  })
})