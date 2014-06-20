## Put comments here that give an overall description of what your
## functions do
# makeCacheMatrix calculates the inverse of a matrix and stores result in cache
# cacheSolve returns the matrix inverse if it exists in cache, otherwise it calculates the inverse and returns it


## Write a short comment describing this function
# makeCacheMatrix generates a list containing four functions: set, get, setinv, getinv
# set: assigns values of x and m to cache
# get: gets the value of x
# setinv: calculates matrix inverse
# getinv: retrives the matrix inverse
#
# EXAMPLE:  dat <- matrix(c(2,3,2,2),nrow=2,ncol=2)
#           MI_info <- makeCacheMatrix(dat)
#
makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinv <- function(solve) m <<- solve
      getinv <- function() m
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## Write a short comment describing this function
# Checks to see if matrix inverse has already been calculated
# if matrix inverse is stored in cache, matrix inverse is retrieved and returned
# otherwise matrix inverse is calculated using solve()
#
# Example: cacheSolve(MI_info)
#
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$getinv()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinv(m)
      m
}