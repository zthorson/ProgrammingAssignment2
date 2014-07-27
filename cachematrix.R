## These functions are a pair of functions that handle storing and calculating 
## matrix inverses.  In a lot of statistical operations, you may calculate 
## the inverse of a given matrix many times, so some method of saving repeated
## computations should be used.  These functions cache known inverses allowing
## saved computations when calcuating repeating inverses.

## This class is a helper class for caching the results of a matrix inversion
## It will not typically be used on its own, since all caching behavior will
## be handled by cachesolve, and this is simply a structure that stores the
## results.
##
## There isn't really a practical reason that we couldn't make get_inverse also
## calculate the inverse or return the cached version, but that wasn't the 
## assignment.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
 
  # Whenever we change the matrix, we should remove the cached inverse
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  set_inverse <- function(new_inverse) inverse <<- new_inverse
  get_inverse <- function() inverse
  
  # Return the list of functions that the cache matrix can call
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}

