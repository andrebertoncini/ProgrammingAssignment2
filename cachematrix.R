## Initially, the function creates a matrix and generate its inverse. 
## After that is done, it generates the inverse of the matrix using the cached data istead of processing it again.

## This function creates a matrix and calculates its inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function () x
	setInverse <- function(solve) m <<- solve
	getInverse <- function () m
	list(set = set, get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}

## This function generates the inverse of the matrix using the cached data istead of processing it again.

cacheSolve <- function(x = matrix(), ...) {
	m <- x$getInverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setInverse(m)
	m
}

