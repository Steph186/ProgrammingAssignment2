## Assignment: Caching the Inverse of a Matrix



## makeCacheMatrix stores and retreive the matrix and its inverse. 
## It allows to retreive the value without the need of computing it more than once.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL # whenever new matrix is saved, it erases previously-stored inverse.
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve # solve - computes the inverse matrix
  getinv <- function() m
  list(set = set, get = get, # returns a list of function (giving the same function name to that function.)
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve catches the values of the matrix and its inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}