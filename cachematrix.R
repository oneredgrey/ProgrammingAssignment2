## This program creates two functions that will create an R object that stores
## a matrix and caches its inverse.


## This function creates a special "vector", which is a list containing a function 
## to set and get the value of the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL      
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function calculates the inverse of the matrix,
## but first checks to see if the inverse was already calculated.  If so, it 
## gets the inverse from the cache.  Otherwise, it calculates the inverse
## and sets the value of the mean so it can be later retrieved.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv = x$getinverse()
  if(!is.null(inv)) {
    message("Let me get that from the cache...")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

