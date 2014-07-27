## These functions creates a Matrix and calculate and store in the cache its inverse.
##



## The next function is the first part of the assignment and creates a Matrix and get its value.

makeCacheMatrix <- function(x = matrix()) {
      # create a null object
      m <- NULL
      # store a matrix and clear the cache
      setMatrix <- function(y) {
            x <<- y
            m <<- NULL
      }
      # get the store matrix and store/get the inverse
      getMatrix <- function() x
      setSolve <- function(solve) m <<- solve
      getSolve <- function() m
      list(setMatrix = setMatrix, getMatrix = getMatrix,
           setSolve = setSolve,
           getSolve = getSolve)
}


## This is the second part of the assignment that computes the inverse if necessary and returns it.

cacheSolve <- function(x, ...) {
      # get the cached value
      m <- x$getSolve()
      #if the cached value exists return it
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      # otherwise calculate the inverse, store and return it.
      dataSolve <- x$getMatrix()
      m <- solve(dataSolve)
      x$setSolve(m)
      m
}


