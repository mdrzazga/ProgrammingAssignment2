## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Crete object holding matrix x and chached invert matrix 

makeCacheMatrix <- function(x = matrix()) {
  invx <- NULL
  set <- function(y) {
    x <<- y
    invx <<- NULL
  }
  get <- function() x
  setinverse <- function(ix) invx <<- ix
  getinverse <- function() invx
  # return list object with  getters and setters
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## function computes the inverse of the special "matrix" returned by makeCacheMatrix . 
##If the inverse has already been calculated, then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ix <- x$getinverse()
  if(!is.null(ix)) {
    message("getting cached data")
    return(ix)
  }
  data <- x$get()
  ## actual solve matrix
  ix <-solve(data, ...)
  x$setinverse(ix)
  ix
}

