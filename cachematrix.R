## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	# setting the value of matrix
	inverse_x <- NULL
	set <- function(y) {
		x <<- y
		inverse_x <<- NULL
	}

	## getting the value of matrix
	get <- function() x

	## setting the inverse of matrix
	setinverse<- function(inverse) inverse_x <<-inverse

	## getting the inverse of matrix
	getinverse <- function() inverse_x

	## setting up variables for access from other functions
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    ## getting the inverse of matrix
 	inverse_x <- x$getinverse()

 	## check to see if the inverse matrix exists
	if (!is.null(inverse_x)) {
		message("getting cached inverse matrix")
		## if so, return the inverse matrix
		return(inverse_x)
	} else {
		## else compute the inverse matrix from the given original matrix
		inverse_x <- solve(x$get())
		## set the computed inverse matrix to be the inverse matrix
		x$setinverse(inverse_x)
		## rethrn the computed inverse matrix
		return(inverse_x)
	}
}
