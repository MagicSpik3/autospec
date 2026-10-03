test_that("destructive rebuild unloads the loaded wealthdv namespace", {
  library(wealthdv)
  expect_true("wealthdv" %in% loadedNamespaces())

  wealthdv:::unload_wealthdv_for_reinstall(destructive = TRUE)

  expect_false("wealthdv" %in% loadedNamespaces())
  expect_false("package:wealthdv" %in% search())

  devtools::load_all(".")
  expect_true("wealthdv" %in% loadedNamespaces())
})
