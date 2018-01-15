## I'm trying to solve assignment2 :)
## Two functions;
## makeCacheMatrix creates the special matrix that could be inversed
## cacheSolve retrieves the inversed matrix from cache (if available),
## or inverses it

## Creates the special inversible matrix
## Returns a list of two functions; setmatrix and getmatrix

makeCacheMatrix <- function(x = matrix()) {
  inversed <- NULL
  setmatrix <- function(y){
    x <<- y
    inversed <<- NULL
    
  }
  getmatrix <- function() x
  setinverse <- function(solve) inversed <<- solve
  getinverse <- function() inversed
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinverse = setinverse, getinverse = getinverse)
}


## Retrieves the inversed matrix from cache
## If the inversed matrix is not in the cache, inverses it using solve()

cacheSolve <- function(x, ...) {
  inversed <- x$getinverse()
  if(!is.null(inversed)){
    print("Getting cached inversed matrix")
    return(inversed)
  }
  m = x$getmatrix()
  inversed <- solve(m, ...)
  x$setinverse(inversed)
  ## Return a matrix that is the inverse of 'x'
  inversed
}
