## Functions in this file allow to create a spcial marix which stores in cache the inverse of the matrix

## given a matrix x, makes a special version of x which can store a cached version of the inverse of x
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


## calls solve for the matrix x, first checking the cache to determine if solve has already been called and it's result
## cached.
cacheSolve <- function(x, ...) {
	## check if the inverse has already been calculated and cached.
	## if so, return
	inverse <- x$getInverse()
	if(!is.null(inverse)) {
		message("getting cached data")
		return(inverse)
	}
	
	## if we get here, nothing found in cache so call solve and cache the result
	matrix <- x$get()
	inverse <- solve(matrix, ...)
	x$setInverse(inverse)
	
	## return the inverse
	inverse
}
