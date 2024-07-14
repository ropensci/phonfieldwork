test_that("df_to_tier", {
  time_start <- c(0.00000000, 0.01246583, 0.24781914, 0.39552363, 0.51157715)
  time_end <- c(0.01246583, 0.24781914, 0.39552363, 0.51157715, 0.65267574)
  content <- c("", "T", "E", "S", "T")
  my_df <- data.frame(id = 1:5, time_start, time_end, content)
  tg <- df_to_tier(my_df,
                   system.file("extdata", "test.TextGrid", package = "phonfieldwork"),
                   overwrite = FALSE
  )

  readLines(system.file("extdata", "test.TextGrid", package = "phonfieldwork"))

  expect_error(
    df_to_tier(my_df[-2],
      system.file("extdata", "test.TextGrid",
        package = "phonfieldwork"
      ),
      overwrite = FALSE
    ),
    paste0(
      'df columns should have the folowing names: "content"',
      '"time_start" and "time_end"'
    )
  )
  expect_length(tg, 108)
})
