## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache 
## its inverse

makeCacheMatrix <- function(x = matrix()) {
  w <- NULL
  set <- function(y) {
    x <<- y
    w <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) w <<- mean
  getinverse <- function() w
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  w <- x$getinverse()
  if(!is.null(w)) {
    message("getting cached data")
    return(w)
  }
  data <- x$get()
  w <- solve(data, ...)
  x$setinverse(w)
  w
}
