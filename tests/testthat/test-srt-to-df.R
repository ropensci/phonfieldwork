test_that("srt_to_df", {
  t <- srt_to_df(system.file("extdata", "test.srt", package = "phonfieldwork"))
  expect_true(nrow(t) == 4)
})
