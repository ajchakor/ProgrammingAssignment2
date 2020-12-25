## Put comments here that give an overall description of what your
## functions do

## function to create a matrix object that caches its inverse
makeCacheMatrix <- function(x = matrix()) {

  
  temp <- NULL
  set <- function(y){
    x <<- y
    temp <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) temp <<- inverse
  getInverse <- function() temp 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## function to calculate the inverse of the matrix returned by makeCacheMatrix function
## or retrieve the cached inverse if the inverse is already calculated
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  temp <- x$getInverse()
  if(!is.null(temp)){
    message("getting cached data")
    return(temp)
  }
  mat <- x$get()
  temp <- solve(mat,...)
  x$setInverse(temp)
  temp
  
}
