## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  w <- NULL
  set <- function(y){         #child function to set the matrix
    x <<- y
    w <<- NULL
  }
  get <- function() {x}      #function to get the matrix
  setInverse <- function(inverse) {w <<- inverse}
  getInverse <- function() {w}    #function to get the inverse of the matrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  w <- x$getInverse()
  if(!is.null(w)){       #checks for inverse
    message("getting cached data")  #if already cached
    return(w)
  }
  mat <- x$get()
  w <- solve(mat, ...)   #solve function to find the inverse of square matrices
  x$setInverse(w)
  w      #returns the inverse of the matrix
}
