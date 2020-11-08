test_that("annotate_textgrid_AND_tier_to_df", {
  textgrid <- system.file("extdata", "test.TextGrid", package = "phonfieldwork")
  textgrid2 <- readLines(textgrid)

  t1 <- tier_to_df(annotate_textgrid(
    annotation = c("", "t", "e", "s", "t"),
    textgrid = textgrid,
    tier = 2, write = FALSE
  ),
  tier = 2
  )
  t2 <- tier_to_df(annotate_textgrid(
    annotation = c("", "t", "e", "s", "t"),
    textgrid = textgrid2,
    tier = 2, write = FALSE
  ),
  tier = 2
  )
  expect_equal(t2$content, c("", "t", "e", "s", "t"))
  expect_equal(t2$content, c("", "t", "e", "s", "t"))
})
