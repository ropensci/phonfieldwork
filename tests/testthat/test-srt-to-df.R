t <- srt_to_df(system.file("extdata", "test.srt", package = "phonfieldwork"))

test_that("srt_to_df", {
  expect_true(nrow(t) == 4)
})
