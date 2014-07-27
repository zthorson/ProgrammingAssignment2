# This is a basic unit testing framework to help verify the code being
# written for the cacheing function.  It is mostly a list of requirements
# that the code must perform to in order to be accepted by the grader.

source('../../cachematrix.R')

# Tests for the underlying cache matrix. Mostly we are concerned that the mean is cleared
# whenever the underlying list is modified.
# Some of these tests aren't necessary, but were good practice.

context("MakeCacheMatrix")

test_that("Inverse is initialized to zero", {
    matrix <- makeCacheMatrix()
    expect_that(matrix$get_inverse(), is_null())
})

test_that("Can set and get cached inverse", {
    matrix <- makeCacheMatrix()
    expect_that(matrix$get_inverse(), is_null())
    test_inverse <- matrix(c(2, 4, 3, 1), nrow=2, ncol=2)
    
    matrix$set_inverse(test_inverse)
    expect_that(matrix$get_inverse(), equals(test_inverse))
    
})

test_that("Matrix is initialized properly", {
    matrix <- makeCacheMatrix()
    expect_that(matrix$get(), is_a("matrix"))
})

test_that("Can get and set underlying matrix", {
    test_inverse <- matrix(c(2, 4, 3, 1), nrow=2, ncol=2)
    matrix <- makeCacheMatrix(test_inverse)
    expect_that(matrix$get(), equals(test_inverse))
    
    test_inverse2 <- matrix(c(1,2,3,4), nrow=2, ncol=2)
    matrix$set(test_inverse2)
    expect_that(matrix$get(), equals(test_inverse2))
})

test_that("Inverse is reset when matrix is modified", {
    test_inverse <- matrix(c(2, 4, 3, 1), nrow=2, ncol=2)
    matrix <- makeCacheMatrix(test_inverse)
    
    matrix$set_inverse(matrix(c(1,2,3,4), nrow=2, ncol=2))
    expect_that(matrix$get_inverse, not(is_null()))
    
    matrix$set(matrix(c(1,2,3,4), nrow=2, ncol=2))
    expect_that(matrix$get_inverse(), is_null())
})


# Tests for the actual solving of the matrix inverse
# We can make sure the mean is calculated, but the only way to unit test
# the performance difference is to time it, which can be fragile for a unit
# test, but is done here anyway for this assignment.

context("CacheSolve")

test_that("Inverse is calculated when calling solve for first time", {
    test <- matrix(c(2, 4, 3, 1, 5, 7, 8, 9, 10), nrow=3, ncol=3)
    matrix <- makeCacheMatrix(test)
    
    # Calculate the inverse so we can make sure it is correct
    test_inv <- solve(test)
    
    expect_that(matrix$get_inverse(), is_null())
    cacheSolve(matrix)
    expect_that(matrix$get_inverse(), equals(test_inv))
    
})

test_that("Inverse is recalculated when changing the underlying matrix", {
    test <- matrix(c(2, 4, 3, 1, 5, 7, 8, 9, 10), nrow=3, ncol=3)
    # Calculate the inverse so we can make sure it is correct
    test_inv <- solve(test)
    
    matrix <- makeCacheMatrix(test)
    expect_that(matrix$get_inverse(), is_null())
    cacheSolve(matrix)
    expect_that(matrix$get_inverse(), equals(test_inv))
    
    test2 <- matrix(c(1,2,3,4), nrow=2, ncol=2)
    test2_inv <- solve(test2)
    matrix$set(test2)
    
    expect_that(matrix$get_inverse(), is_null())
    expect_that(cacheSolve(matrix), equals(test2_inv))
    expect_that(matrix$get_inverse(), equals(test2_inv))
})

test_that("Stored inverse is returned if calling solve again", {
    # This uses the trick of checking for the message that gets called when
    # the stored inverse is used.  It will break if the message is not
    # displayed
    
    test <- matrix(c(2, 4, 3, 1, 5, 7, 8, 9, 10), nrow=3, ncol=3)
    # Calculate the inverse so we can make sure it is correct
    test_inv <- solve(test)
    
    matrix <- makeCacheMatrix(test)
    expect_that(matrix$get_inverse(), is_null())
    expect_that(cacheSolve(matrix), equals(test_inv))
    expect_that(matrix$get_inverse(), equals(test_inv))
    
    # Now the dirty trick of checking for the message
    expect_that(cacheSolve(matrix), shows_message("Getting cached inverse"))
    
})

# Note this is a fragile test, only valid for the PC it was written on.
# Really it is a terrible way to do this, but I have to look more into timing R
# functions to write a better one.
test_that("Cached inverse speeds up computation for large matrices", {
    test <- matrix(rnorm(1000000), ncol=1000, nrow=1000)
    m <- makeCacheMatrix(test)
    
    # First test will take a while since it must find the inverse
    expect_that(cacheSolve(m), not(takes_less_than(.5)))
    
    # All the following should be much quicker
    expect_that(cacheSolve(m), takes_less_than(0.5))
    expect_that(cacheSolve(m), takes_less_than(0.5))
    expect_that(cacheSolve(m), takes_less_than(0.5))
})

