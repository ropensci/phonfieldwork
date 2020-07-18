library(phonfieldwork)
context("Tests for 'create_sound_play()' function")

test_that("tier_to_df", {
  expect_equal(nchar(create_sound_play("a.wav")), 117)
})

