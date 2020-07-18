library(phonfieldwork)
context("Tests for 'create_sound_play()' function")

test_that("create_sound_play", {
  expect_equal(nchar(create_sound_play("a.wav")), 117)
})

