
## Put comments here that give an overall description of what your
## functions do

## The function has the structure to absorb a matrix and an
## inverse matrix via the set and setinv functions.
## It also contains a flag to find out, if the matrix has changed.


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  ## inverted matrix
  changed <- FALSE   ## Indicator if matrix has changed
  set <- function(y) {
    x <<- y
    inv <<- NULL
    haschanged <<- TRUE
  }
  get <- function() x
  setinv <- function(inverse) {
    inv <<- inverse
    changed <<- FALSE
  }
  getinv <- function() inv
  haschanged <- function() changed
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function tests if there is already an inverted matrix
## in the matrix object and also tests if the matrix has changed.
## If it has changed, a new inverse matrix is created and
## stored in the cahce matrix object. Otherwise, the
## cached inverse matrix is returned.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv) && !x$haschanged()) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat)
  x$setinv(inv)
  inv
}