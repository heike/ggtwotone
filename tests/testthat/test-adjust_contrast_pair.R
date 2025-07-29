test_that("adjust_contrast_pair gives warning and fallback for invalid color", {
  expect_warning(
    result <- adjust_contrast_pair("notacolor", quiet = FALSE),
    regexp = "Could not convert"
  )
  expect_equal(result$light, "#FFFFFF")
  expect_equal(result$dark, "#000000")
  expect_true(is.na(result$contrast))
})

test_that("adjust_contrast_pair fails silently when quiet = TRUE", {
  expect_silent(
    result <- adjust_contrast_pair("notacolor", quiet = TRUE)
  )
  expect_equal(result$light, "#FFFFFF")
  expect_equal(result$dark, "#000000")
  expect_true(is.na(result$contrast))
})

