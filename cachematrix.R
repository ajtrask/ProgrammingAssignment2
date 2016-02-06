## These functions (makeCacheMatrix and cacheSolve) implement
## a matrix "object" with the ability to cache its inverse

## makeCacheMatrix creates a matrix "object" that has funtions to:
##   set and get the matrix
##   set and get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  minv <- NULL
  
  ## set the matrix and NULL the inverse placeholder
  set <- function(y) {
    x <<- y
    minv <<- NULL
  }
  
  ## get the matrix
  get <- function() x
  
  ## set the inverse cache
  setinv <- function(solve) minv <<- solve
  
  ## get the inverse
  getinv <- function() minv
  
  ## create the list of functions
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## cacheSolve calculates the inverse of the matrix "object"
## created in makeCacheMatrix by:
##     checking if the inverse has already been calculated
##     and getting the inverse from the cache
##     or calculating the invese and storing in the cache
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  minv <- x$getinv()
  
  ## if the inverse exists, return the cached value
  if(!is.null(minv)) {
    message("returning cached value")
    return(minv)
  }
  
  ## otherwise, compute and store the cache and return the value
  a <- x$get() ## get the matrix
  minv <- solve(a, ...)  ## compute the inverse
  x$setinv(minv) ## cache the inverse
  minv ## return the inverse
}
