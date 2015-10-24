# Matrix inversion is usually a costly computation 
# and there may be some benefit to caching the inverse 
# of a matrix rather than compute it repeatedly.
# Your assignment is to write a pair of functions that 
# cache the inverse of a matrix.

# Write the following functions:
  
# makeCacheMatrix: This function creates a special "matrix" 
# object that can cache its inverse.
# cacheSolve: This function computes the inverse of the 
# special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should 
# retrieve the inverse from the cache.
# Computing the inverse of a square matrix can be done with the 
# solve function in R. For example, if X is a square invertible matrix, 
# then solve(X) returns its inverse.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

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


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.  It is assumed that matrix passed is a sqare matrix.

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

## Output
#> x <- matrix(1:6, nrow = 2, ncol = 2)
#> x
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4

#> m <- makeCacheMatrix(x)
#> m$get()
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4

#> cacheSolve(m)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

#> cacheSolve(m)
# getting cached data
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5