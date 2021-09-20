## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## library(MASS) is ued to calculate inverse for non squared as well as square matrices
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y)
				{
				x <<- y
				inv <<- NULL	
				}
	get <- function()x
	setinv <- function(inverse) inv <<- inverse
	getinv <- function(){
					inver <- ginv(x)
					inver%*%x
				  }
	list(set = set, get = get, 
		setinv = setinv,
		getinv = getinv)
}


## Write a short comment describing this function
## this is used to get the cache data
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
	if(!is.null(inv)){
			message("getting cached data!")
			return(inv)		#return inverse value
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
	inv 	##return a matrix that is inverse of 'x'
}
