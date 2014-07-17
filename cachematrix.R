## makeCacheMatrix creates the special matrix object and creates it's inverse, "inverse", to be cached
## cacheSolve returns the inverted special matrix if it has already been calculated;nif not, it is 
## computed using the solve() function

## Similar to the example, makeCacheMatrix has 4 public methods for interaction: set, get, setinverse
## and getinverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(matrix) inverse <<- matrix
  getinverse <- function () inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}

## returns message with inverted matrix if already computed, if not, computes and returns inverted 
## matrix

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("Getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
