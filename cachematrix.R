## These functions take a user supplied square matrix into makeCacheMatrix
## The matrix is then cached and then inverted using cacheSolve

## makeCacheMatrix takes a matrix as an argument then saves the matrix into cache
## This is then used by cacheSolve to invert the matrix saved in memory
makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(matrix) m <<- matrix
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## cacheSolve checks that a matrix is saved, then inverts it

cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
        ## Return a matrix that is the inverse of 'x'
}

## Sample inputs that show the functionality of the two functions
inverse <- makeCacheMatrix(matrix(1:4,2,2))
cacheSolve(inverse)
