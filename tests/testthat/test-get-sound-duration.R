d1 <- get_sound_duration(
  sounds_from_folder = system.file("extdata", package = "phonfieldwork")
)

d2 <- get_sound_duration(
  system.file("extdata", "test.wav", package = "phonfieldwork")
)

d3 <- get_sound_duration(
  tuneR::readWave(
    system.file("extdata", "test.wav", package = "phonfieldwork")
  )
)

test_that("get_sound_duration", {
  expect_true(round(sum(d1$duration), 2) == 1.29)
  expect_true(round(d2$duration, 2) == 0.65)
  expect_true(round(d3$duration, 2) == 0.65)
})
