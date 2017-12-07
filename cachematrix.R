## Put comments here that give an overall description of what your
## functions do
## a requirement for Coursera course week three Assignment

## Write a short comment describing this function
## creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) { #defines the function makeCacheMatrix with argument default class is "matrix"
  invs <- NULL              # variable that will hold the value of inverse matrix
  setMatrix <- function(y) {      # set function to assign new
    x <<- y                 # holds value of matrix in parent environment
    invs <<- NULL           # If new matrix exist, invs = NULL
  }
  getMatrix <- function() x       # get function will return value of matrix
  setInverse <- function(inverse) invs <<- inverse # assigns value of invs in parent environment
  getInverse <- function() invs                    # gets the value of invs
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)  # make list to easily access elements with $
}


## Write a short comment describing this function
## computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invs <- x$getInverse()
  if(!is.null(invs)) {
    message("getting cached data")
    return(invs)
  }
  data <- x$getMatrix()
  invs <- solve(data, ...)
  x$setInverse(invs)
  invs
}
