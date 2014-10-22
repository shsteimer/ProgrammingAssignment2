## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}
	get <- function() {
		x
	}
	getInverse <- function() {
		inverse
	}
	setInverse <- function(inv) {
		inverse <<- inv
	}
	list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
	inverse <- x$getInverse()
	if(!is.null(inverse)) {
		message("getting cached data")
		return(inverse)
	}
	
	matrix <- x$get()
	inverse <- solve(matrix, ...)
	x$setInverse(inverse)
	inverse
}
