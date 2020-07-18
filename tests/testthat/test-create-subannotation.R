library(phonfieldwork)
context("Tests for 'create_subannotation()' functions")

tg <- system.file("extdata", "test.TextGrid", package = "phonfieldwork")
tg1 <- create_subannotation(tg, tier = 1, overwrite = FALSE)
tg2 <- create_subannotation(readLines(tg), tier = 1, overwrite = FALSE)

df1 <- textgrid_to_df(tg1)
df2 <- textgrid_to_df(tg2)

test_that("create_subannotation", {
  expect_true(nrow(df1) == 30)
  expect_true(nrow(df2) == 30)
})
