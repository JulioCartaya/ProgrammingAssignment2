## Put comments here that give an overall description of what your
## functions do

## Creates a special "matrix" that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
  set <- function(y) {
          x <<- y
          m <<- NULL
  }
  get <- function() x
	setinv <- function(inv) m <<- inv
	getinv <- function() m
	list(set = set, get = get,
	     setinv = setinv,
	     getinv = getinv)
}


## Computes the inverse of the "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has 
## not changed) then cachesolve retrieves the inverse from the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
				m <- x$getinv()
				if (!is.null(m)) {
								message("getting cached data")
                m
								return(m)
				}
        ## inverse has never been calculated, or 
        ## the special matrix 'x' has changed
        temp <- x$get()
				m <- solve(temp,...)
				x$setinv(m)
				m
}
