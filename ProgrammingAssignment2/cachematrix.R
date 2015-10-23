## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  inverseMatrix <- NULL
  
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(x) inverseMatrix <<- x
  
  getInverse <- function() inverseMatrix
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverseMatrix <- x$getInverse()
  
  if(!is.null(inverseMatrix)) {
    message("getting cached data")
    return(inverseMatrix)
  }
  
  data <- x$get()
  
  inverseMatrix <- solve(data)
  
  x$setInverse(inverseMatrix)
  
  inverseMatrix
  
}
