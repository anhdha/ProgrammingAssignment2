## makeCacheMatrix and cacheSolve save the inverse of a matrix so that if the matrix
## does not change, they enable one to use it without having to inverse the matrix over
## and over again.


## makeCacheMatrix stores a matrix and cache its inverse. It contains a list of functions
## (get(), set(), get_inv(), set_inv()). 
## get() returns the matrix 'x' that was stored in the makeCacheMatrix function. 
## set() changes the matrix 'x'('x <<- y' replaces x with y 
## in the main function makeCacheMatrix). It also resets the value of inv back to NULL. 
## get_inv() and set_inv() returns and stores the value of the input into makeCacheMatrix, respectively.
## list is the result of the main function, containing 4 functions as its elements.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  get <- function() x
  set <- function(y) {
    inv <<- NULL
    x <<- y
  }
  get_inv <- function() inv
  set_inv <- function(solve) inv <<- solve
  list(set=set, get=get, get_inv=get_inv, set_inv=set_inv)
}


## caheSolve computes a matrix that is the inverse of 'x'. If the inverse of 'x' has
## been calculated and stored in makeCacheMatrix (not NULL), cacheSolve would pull out the cached inverse.
## If the inverse in makeCacheMatrix was NULL, caheSolve would get the matrix 'x',
## compute the inverse matrix and store this in makeCacheMatrix using set_inv().

cacheSolve <- function(x, ...) {
  inv <- x$get_inv()
  if (!is.null(inv)) {
    message("getting cache data")
    inv
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$set_inv(inv)
  inv
}
