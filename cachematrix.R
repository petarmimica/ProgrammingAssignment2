## Returns a list containing these functions:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}
	
	get <- function() x
	setinverse <- function(inv) inverse <<- inv
	getinverse <- function() inverse
	
	list(set = set, get=get, setinverse=setinverse, getinverse = getinverse)
}


## Returns the inverse of a "matrix" created using the above function.
## It first checks to see if the mean has already been calculated (and the matrix has not changed). If so, it gets the inverse from the cache, informs the user and skips the calulation. Otherwise, it calculates the inverse and sets the cache using setinverse function.
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if (!is.null(inv)) {
        	message("getting cached data")
        	return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
