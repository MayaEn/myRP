test_that("na_count() count's missing values", {
  expect_equal(na_count(c(1, 2, NA)), 1)
})
