# library(phonfieldwork)
# context("Tests for 'create_glossed_document()' functions")
#
# my_tmp1 <- tempdir()
# create_glossed_document("../../vignettes/files/zilo_test.flextext",
#                         output_dir = my_tmp1)
#
# my_tmp2 <- tempdir()
# create_glossed_document(
#   flextext_to_df("../../vignettes/files/zilo_test.flextext"),
#   output_dir = my_tmp2)
#
# test_that("create_glossed_document", {
#   expect_true(file.exists(paste0(my_tmp1, "/glossed_document.docx")))
#   expect_true(file.exists(paste0(my_tmp2, "/glossed_document.docx")))
# })
#
