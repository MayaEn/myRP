#' Count Missing Values
#'
#' @param x A vector or list
#'
#' @return A numberical vector
#' @export
#'
#' @examples
#' na_count(c(1, 2, NA, NA))
na_count <- function(x) {
  base::sum(base::is.na(x))
}

# Test when x is a vector with no NAs
test_that("na_count returns 0 when there are no NAs", {
  x <- c(1, 2, 3, 4, 5)
  expect_equal(na_count(x), 0)
})

# Test when x is a vector with NAs
test_that("na_count returns the correct count of NAs in a vector", {
  x <- c(1, NA, 3, NA, 5)
  expect_equal(na_count(x), 2)
})

# Test when x is a matrix with no NAs
test_that("na_count returns 0 when there are no NAs in a matrix", {
  x <- matrix(1:9, nrow = 3, ncol = 3)
  expect_equal(na_count(x), 0)
})

# Test when x is a matrix with NAs
test_that("na_count returns the correct count of NAs in a matrix", {
  x <- matrix(c(1, 2, NA, NA, 5, 6, NA, 8, 9), nrow = 3, ncol = 3)
  expect_equal(na_count(x), 3)
})

# Test when x is a data frame with no NAs
test_that("na_count returns 0 when there are no NAs in a data frame", {
  x <- data.frame(a = 1:3, b = 4:6, c = 7:9)
  expect_equal(na_count(x), 0)
})

# Test when x is a data frame with NAs
test_that("na_count returns the correct count of NAs in a data frame", {
  x <- data.frame(a = c(1, NA, 3), b = c(NA, 5, 6), c = c(7, 8, NA))
  expect_equal(na_count(x), 3)
})

# Test when x is NULL
test_that("na_count returns 0 when x is NULL", {
  x <- NULL
  expect_equal(na_count(x), 0)
})

# Test when x is an empty vector
test_that("na_count returns 0 when x is an empty vector", {
  x <- numeric(0)
  expect_equal(na_count(x), 0)
})

# Test when x is an empty matrix
test_that("na_count returns 0 when x is an empty matrix", {
  x <- matrix(nrow = 0, ncol = 0)
  expect_equal(na_count(x), 0)
})

# Test when x is an empty data frame
test_that("na_count returns 0 when x is an empty data frame", {
  x <- data.frame()
  expect_equal(na_count(x), 0)
})
