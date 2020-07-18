library(phonfieldwork)
context("Tests for 'create_image_look_up()' function")

test_that("create_image_look_up", {
  expect_equal(nchar(create_image_look_up("a.png")), 121)
  expect_error(create_image_look_up("a.png", img_caption = 1:2),
               paste0("It looks like the img_src variable contains 1 objects ",
                      "and the img_caption variable contains 2"))
})

