
## The makeCacheMatrix function creates a special "matrix" object that can cache 
## its inverse. The special matrix is really a list containing a function to:
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse 
# get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) i <<- solve
  getInverse <- function() i
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The cacheSolve function computes the inverse of the special "matrix" returned by 
## the makeCacheMatrix function above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cacheSolve function should retrieve the 
## inverse from the cache and skip the computation. Otherwise, it calculates the 
## inverse of the matrix and sets the value of the inverse matrix in the cache via 
## the setInverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  i <- x$getInverse()
  
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
