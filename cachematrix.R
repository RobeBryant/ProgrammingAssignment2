## Create a special matrix which is a list containing a function to set the value of 
## the matrix, get the value of the matrix, set the value of the inverse matrix, get 
## the value of the inverse matrix

makeMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(invmatrix) m <<- invmatrix
  getinv <- function() m
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)
}

## Check to see if the inverse matrix has already been calculated. If so, it gets 
## the inverse matrix from the cache and skips the computation. Otherwise, it 
## calculates the inverse matrix of the data and sets the value in the cache via the 
## setinv function.

cachematrix <- function(x, ...) {
  m <- x$getinv()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}