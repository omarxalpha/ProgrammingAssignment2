## Put comments here that give an overall description of what your
## functions do

#  The function makeCacheMatrix creates a especial "matrix", 
## which is really a list containing a function to

#set the value of the vector
#get the value of the vector
#set the value of the inverse matrix, with the function "solve()"
#get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)  
}

## The following function calculates the inverse of the special 
##"matrix" created with the above function. However, it first 
## checks to see if the inverse matrix has already been calculated.
## If so, it gets the mean from the cache and skips the computation. 
## Otherwise, it calculates the mean of the data and sets the value 
## of the mean in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}
