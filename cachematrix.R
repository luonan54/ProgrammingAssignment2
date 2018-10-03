## Caching the inverse of a matrix:
## There are in total 2 functions

## The function below creates a "matrix" object whose inverse can be cached.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
        x <<- y
        inv <<- NULL
      }
      get <- function() x
      setInverse <- function(inverse) inv <<- inverse
      getInverse <- function() inv
      list(set = set,
           get = get,
           setInverse = setInverse, getInverse = getInverse)
      }


## The function below returns the inverse of "matrix"
## created above. And if the inverse has already been calculated,
## it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if (!is.null(inv)) {
          message("getting cached data")
          return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
