##Because matrix inversion is a costly computation, storing the inverse in memory for subsequent computations 
##can be useful. This pair of functions caches a matrix inverse.  

## makeCacheMatrix creates a list of functions that:
## 1. sets the matrix & nulls its inverse
## 2. gets the matrix & nulls is inverse
## 3. sets the matrix inverse 
## 4. gets the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() {
    x
    }
  setinverse <- function(inverse) {
    inv <<- inverse
  }
  getinverse <- function(){
    inv
  }
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Returns a matrix that is the inverse of 'x', but first checks if the inverse has already been calculated.
## If so, it will be retrieved from cache. Otherwise, inverse will be calculated & set in cache via the 
##setinverse function.
cacheSolve <- function(x, ...) {
        
  inv <- x$getinverse()
  if(!is.null(inv)) {       ##returns inverse if inv is not null
    message("getting cached data")
    return(inv)
  }
  data <- x$get()         ##if inv = null, calculate the inverse
  inv <- solve(data, ...)
  x$setinverse(inv)      ##set inv to new calculated inverse & return the value
  inv
}
