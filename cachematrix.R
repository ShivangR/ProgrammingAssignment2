## makeCacheMatrix creates a list containing a function to
## 1-->Set the value of matrix
## 2-->Get the value of tmatrix
## 3-->Set the value of inverse of  matrix
## 4-->Get the value of inverse of  matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The function checks if the inverse is already complted 
## If yes, it fetches the result and skips the calculation of inverse.
## otherwise, it calculates the inverse, sets the value in cache through setinverse function.

cacheSolve <- function(x, ...) {

  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m        ## Return a matrix that is the inverse of 'x'
}
