test_that("df_to_tier", {
  
  df <- exb_to_df(system.file("extdata", "demo_Beckhams.exb", package = "phonfieldwork"))
  df_to_exb(df = df, name = 'Beckhams', outputPath = 'beck.xml')
  
  expect_warning(
    df_to_exb(df = df, name = 'Beckhams', outputPath = 'beck.xml'),
    paste0(
      'Missing timestamps in rows:  61 149 62 150'
    )
  )
  
  expect_true(file.exists('beck.xml'))
  
  file.remove('beck.xml')
  
})
