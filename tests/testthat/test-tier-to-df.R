file_1 <- system.file("extdata", "test.TextGrid", package = "phonfieldwork")
file_2 <- readLines(system.file("extdata", "test.TextGrid",
  package = "phonfieldwork"
))
test_that("tier_to_df", {
  expect_equal(tier_to_df(file_1)[, 1], 1:5)
  expect_equal(tier_to_df(file_1, tier = "intervals")[, 1], 1:5)
  expect_equal(tier_to_df(file_2)[, 1], 1:5)
  expect_equal(tier_to_df(file_1, tier = 3)[, 1], 1:4) # point tier
})
