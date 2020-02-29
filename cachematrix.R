## These two functions will cache the inverse
## of a matrix. The first function will create
## a matrix object that can cache its inverse.

## Function that creates a matrix object.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
}
  get <- function()x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)

## This function calculates the inverse of 
## the matrix returned by the first function:
## "makeCacheMatrix"  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  mat <- x$get()
  m <- solve(mat,...)
  x$setInverse(m)  
}
