##This fucntion is to caching the inverse of a matrix;
##Because inversion of matrices are costly in term of computing
##so caching the inversion can be better on the runtime rather than
##inverting a matrix directly.

##This function creates a special "matrix" object that can 
##cache its own inverse. 


makeCacheMatrix <- function(x = matrix()) {
  
  invo <- NULL
  set <- function(y) {
    x <<- y
    invo <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) invo <<- inverse
  getInverse <- function() inv
  list(set = set, 
       get = get, 
       setINverse = setInverse,
       getInverse = getInverse)
}

## This function computes the inverse of the "matrix" created by
##above function. 

cacheSolve <- function(x, ...) {
       
  invo <- x$getInverse()
  if (!is.null(inv)){
    message("getting cached data!@_@")
    return(invo)
  }
  math <- x$get()
  invo <- solve(math, ...)
  x$setInverse(invo)
  invo
}

