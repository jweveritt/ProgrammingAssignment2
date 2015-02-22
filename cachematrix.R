## These functions are the submissions for Programming Assignment2.
## They create an "matrix" object which can cache its inverse as well as
## do the actual inverse setting and calculation.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL        
  set <- function(y) {  
    x <<- y                 
    inv <<- NULL         
  }         
  get <- function() x 
  
  setinverse <- function(inverse) 
    inv <<- inverse 
  
  getinverse <- function() inv  
  
  list(set = set,
       get = get,              
       getinverse = getinverse,
       setinverse = setinverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("cahce is populated, using cache")
    return(inv)
  }
  
  message("cache is not populated, calculating inverse")
  inv <- solve(x$get())
  x$setinverse(inv)
  inv
}
