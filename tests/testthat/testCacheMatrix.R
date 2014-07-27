# This is a basic unit testing framework to help verify the code being
# written for the cacheing function.  It is mostly a list of requirements
# that the code must perform to in order to be accepted by the grader.

source('../../cachematrix.R')

# Tests for the underlying cache matrix. Mostly we are concerned that the mean is cleared
# whenever the underlying list is modified.
# Some of these tests aren't necessary, but were good practice.

context("MakeCacheMatrix")

test_that("Mean is initialized", {
  matrix <- makeCacheMatrix()
  expect_that(matrix$get_inverse(), is_null())
})

test_that("Inversion can set and get", {
  matrix <- makeCacheMatrix()
  expect_that(matrix$get_inverse(), is_null())
  test_inverse <- matrix(c(2, 4, 3, 1, 5, 7), nrow=3, ncol=2)
  
  matrix$set_inverse(test_inverse)
  expect_that(matrix$get_inverse(), equals(test_inverse))
  
})

test_that("Matrix is initialized", {
  matrix <- makeCacheMatrix()
  
  expect_that(matrix$get(), is_a(matrix))
})

test_that("Matrix value can set and get", {
  test_inverse <- matrix(c(2, 4, 3, 1, 5, 7), nrow=3, ncol=2)
  matrix <- makeCacheMatrix(test_inverse)
  
  expect_that(matrix$get(), equals(test_inverse))
  
  test_inverse2 <- matrix(c(1,2,3,4), nrow=2, ncol=2)
  matrix.set(test_inverse2)
  
  expect_that(matrix$get(), equals(test_inverse2))
  
})

test_that("Mean is reset when matrix is modified", {
  test_inverse <- matrix(c(2, 4, 3, 1, 5, 7), nrow=3, ncol=2)
  matrix <- makeCacheMatrix(test_inverse)
  
  matrix$set_inverse(matrix(c(1,2,3,4), nrow=2, ncol=2))
  expect_that(matrix$get_inverse, not(is_null()))
  
  matrix$set(matrix(c(1,2,3,4,5,6), nrow=2, ncol=3))
  expect_that(matrix$get_inverse, is_null())
})
  

# Tests for the actual solving of the matrix inverse
# We can make sure the mean is calculated, but the only way to unit test
# the performance difference is to time it, which can be fragile for a unit
# test, but is done here anyway for this assignment.

context("CacheSolve")

test_that("Mean is calculated when calling solve for first time", {
  
})

test_that("Stored mean speeds up computation time for large matrices", {
  
})

