test_that("df_to_tier", {

  df <- exb_to_df(system.file("extdata", "demo_Beckhams.exb", package = "phonfieldwork"))
  tmp_dir <- tempdir()

  expect_warning(
    df_to_exb(df = df, name = 'Beckhams', output_file = "beck.xml", output_dir = tmp_dir),
    paste0(
      'Missing timestamps in rows:  61 149 62 150'
    )
  )

  expect_true(file.exists(paste0(tmp_dir, '/beck.xml')))
  unlink(tmp_dir)
})
