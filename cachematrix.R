## Put comments here that give an overall description of what your
## functions do

## function for make cache matrix

makeCacheMatrix <- function(x = matrix()) {
	reverse <- NULL
	set <- function(y) { ##set new values for matrix
		x <<- y ##set new arguments for matrix
		reverse <<- NULL ##set null to reverse matrix
	}
	get <- function() x ##get default matrix
	setReverse <- function(rev_mat) reverse <<- rev_mat ##set new value for reverse matrix
	getReverse <- function() reverse ##get reverse matrix value
	list(set = set, get = get, setReverse = setReverse, getReverse = getReverse) ##return list of functions for updating values of cache
}




cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        reverse <- x$getReverse()##get value of reverse matrix
        if(!is.null(reverse)) { ##check if something is stored in reverse matrix cache
        	message("getting cached data")
        	return(reverse)
        }
        data <- x$get() ##get value of default matrix
        reverse <- solve(data)##making reverse matrix
        x$setReverse(reverse)##set new value in reverse matrix cache
        reverse ##return reverse matrix
}
