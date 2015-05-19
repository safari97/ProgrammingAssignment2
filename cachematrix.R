## Put comments here that give an overall description of what your
## functions do

## The function makeCacheMatrix creates a speciatial object, a list of functions 
## that can cache its inverse
makeCacheMatrix <- function(M = matrix()) {

     Inv <- NULL
     set <- function(y) {
          M <<- y
          Inv <<- NULL
     }
     get <- function() M
     setInv <- function(mean) Inv <<- inverse
     getInv <- function() Inv
     list(set = set, get = get,
          setInv = setInv,
          getInv = getInv)
}

## cacheSolve computes the inverse of the special matrix. 
## If the inverse exists, it will retrieve the inverse from the cache
cacheSolve <- function(M, ...) {
        ## Return a matrix that is the inverse of 'x'
     Inv <- M$getInv()
     if(!is.null(Inv)) {
          message("getting cached data")
          return(Inv)
     }
     data <- M$get()
     Inv <- solve(data, ...)
     x$setInv(Inv)
     Inv
}
