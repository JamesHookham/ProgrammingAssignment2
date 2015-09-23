## The following function is the equivalent of the makeVector function in the example code
## makeCacheMatrix 
##   - sets the value of the matrix
##   - gets the value of the matrix
##   - sets the value of the inverse matrix
##   - gets the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setInverse <- function(solve) m <<- solve
     getInverse <- function() m
     list(set = set, get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}

## Similarly to the example, cacheSolve inverts the matrix x and either reads it from 
## the cache or not

cacheSolve <- function(x, ...) {
     m <- x$getInverse()
     if(!is.null(m)) {
          message("getting inverse")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setInverse(m)
     m
}