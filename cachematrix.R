## This piece of code caches the inverse of a matrix

## Create a special R matrix object capable of cache-ing 
## its own inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        invx <- NULL
	set <- function(A) {
		x <<- A
		invx <<- NULL
	}
	get <- function() {
		return(x)
	}
	setInverse <- function(inv) {
		invx <<- inv
	}
	getInverse <- function() {
		return(invx)
	}
	list(set = set, get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}


## get the inverse of matrix from cache if available
## otherwise find the inverse and copy it to cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invx <- x$getInverse()
	#message(is.null(invx))
	if (!is.null(invx)) {
		#message("getting cached data")
		return(invx)
	}
	data <- x$get()
	# matrix supplied is always invertible
	invx <- solve(data)
	x$setInverse(invx)
	#message(x$getInverse())
	invx
}
