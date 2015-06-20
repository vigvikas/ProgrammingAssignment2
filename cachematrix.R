## These pair of functions work together to cache the inverse of matrix. If the cache exists
## cache is used without computing; hence saving computational time.

## makeCacheMatrix function creates a special "matrix" object which can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- solve(x)  
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve calculates the inverse of the special matrix returned by makeCacheMatrix. If the inverse exists
## and matrix has not been changed, it gives the cached inverse with message. Otherwise it computes the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
