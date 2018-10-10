## Following are two functions.  The first function will create a matrix object, which will cache its inverse.
## The second function will compute the inverse of the matrix object.  If already solved, then will retrieve 
## from cache rather than recalculating.

## Create a matrix object which will calculate the inverse of an input matrix.

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinv <- function(solve) m <<- solve
      getinv <- function() m
      list(set = set, 
           get = get,
           setinv = setinv,
           getinv = getinv)
}


## This function calculates the inverse of makeCacheMatrix function.  If Inverse already calculated, then 
## cacheSolve will retrieve from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          n <- x$getinv()
          if(!is.null(n)) {
              message("getting cached data")
              return(n)
          }
          data <- x$get()
          n <- solve(data, ...)
          x$setinv(n)
          n
}