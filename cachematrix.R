## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	reverse <- NULL
	set <- function(y) {
		x <<- y 
		reverse <<- NULL
	}
	get <- function() x
	setReverse <- function(rev_mat) reverse <<- rev_mat
	getReverse <- function() reverse
	list(set = set, get = get, setReverse = setReverse, getReverse = getReverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        reverse <- x$getReverse()
        if(!is.null(reverse)) {
        	message("getting cached data")
        	return(reverse)
        }
        data <- x$get()
        reverse <- solve(data)
        x$setReverse(reverse)
        reverse
}