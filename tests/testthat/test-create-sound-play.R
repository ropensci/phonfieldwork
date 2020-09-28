test_that("create_sound_play", {
  expect_equal(nchar(create_sound_play("a.wav")), 117)
})
