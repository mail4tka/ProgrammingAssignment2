## Caching a computationally expensive function like inverse matrix may be beneficial.
## The following two functions are used to cache the inverse of a matrix

## makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   set <- function(y){
	x <<- y
	m <<- NULL
   }
   get <- function() x
   setmatrix <- function(solve) m <<- solve
   getmatrix <- function() m
   list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}


## cacheSolve function returns the inverse of the matrix. It first checks if
## the inverse has already been computed. If so, it gets the result and skips the
## computation. If not, it computes the inverse, sets the value in the cache via
## setmatrix function.

cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
   m <- x$getmatrix()
   if(!is.null(m)){
	message("getting cached data")
	return(m)
   }
   data <- x$get()
   m <- solve(data, ...)
   x$setmatrix(m)
   m
}
