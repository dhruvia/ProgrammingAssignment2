## makeCacheMatrix creates a matrix and cacheSolve calculates 
## the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)  
}

## this function looks up the value for inverse and if it is not there 
## then it calculates the value

cacheSolve <- function(x, ...) {
      m <- x$getinv()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
    data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
  ## Return a matrix that is the inverse of 'x'
}
