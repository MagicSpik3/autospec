test_that("destructive rebuild unloads the loaded autospec namespace", {
  library(autospec)
  expect_true("autospec" %in% loadedNamespaces())

  autospec:::unload_autospec_for_reinstall(destructive = TRUE)

  expect_false("autospec" %in% loadedNamespaces())
  expect_false("package:autospec" %in% search())

  devtools::load_all(".")
  expect_true("autospec" %in% loadedNamespaces())
})
