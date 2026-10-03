test_that("the value is kept where the condition holds", {
  df <- data.frame(type = c(2, 3, 2), amount = c(10, -9, NA))

  result <- dv_keep_if(df, df$type == 2, "amount", "kept")

  # Row 2 holds a sentinel but is not kept, so it does not stop the verb.
  expect_identical(result$kept, c(10, 0, NA))
})


test_that("a sentinel that would be kept stops", {
  df <- data.frame(type = 2, amount = -8)

  expect_error(dv_keep_if(df, df$type == 2, "amount", "kept"), "sentinel")
})


test_that("otherwise can be missing", {
  df <- data.frame(type = c(2, 3), amount = c(10, 20))

  expect_identical(
    dv_keep_if(df, df$type == 2, "amount", "kept", otherwise = NA)$kept,
    c(10, NA)
  )
})
