#function_make_cache_matrix
#creates a matrix object that is able to store its invers
library(MASS)



makeCacheMatrix <- function(x = matrix()){
	#set inverse to NULL
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	#get the matrix x
	get <- function() x
	
	setinv <- function(inverse)inv <<- inverse
	
	getinv <- function() {
		inver <- ginv(x)
		inver%*%x
		
	}
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}

#second function gets the cache


cacheSolve <- function(x, ...) {
	
	inv <- x$getin()
	#checks if invers is 0
	if(!is.null(inv)){
		message("getting cached data!")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ....)
	x$setinv(inv)
	#returns matrix that is the inverse of x
	inv}
}
