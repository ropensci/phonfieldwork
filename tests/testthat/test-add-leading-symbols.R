library(phonfieldwork)
context("Tests for 'add_leading_symbols()' function")

test_that("add_leading_symbols", {
  expect_equal(
    add_leading_symbols(1:10),
    c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10")
  )
})
