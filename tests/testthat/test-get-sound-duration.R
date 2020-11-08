test_that("get_sound_duration", {
  d1 <- get_sound_duration(
    system.file("extdata", "test.wav", package = "phonfieldwork")
  )

  d2 <- get_sound_duration(
    tuneR::readWave(
      system.file("extdata", "test.wav", package = "phonfieldwork")
    )
  )

  expect_true(round(d1$duration, 2) == 0.65)
  expect_true(round(d2$duration, 2) == 0.65)
})
