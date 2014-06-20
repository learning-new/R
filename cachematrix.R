## Set of functions to cache matrix operations 
## Only "solve" is currently supported

## Creates list object that contains matrix and cache 
makeCacheMatrix <-function(m = matrix()) {
	solveCache <- NULL
	set <- function(value) {
		m <<- value
		solveCache <<- NULL
	}
	get <- function() {
		return(m)
	}
	setSolveCache <- function(value) {
		solveCache <<- value
	}
	getSolveCache <- function()
	{	
		return(solveCache)
	}
	
	list(set = set, get = get,
		setSolveCache = setSolveCache,
		getSolveCache = getSolveCache) 
}

## Return a matrix that is the inverse cached matrix of 'x'
cacheSolve <-function(x, ...) { 
	result <- x$getSolveCache()
	if(!is.null(result)) {
		return(result)
	}
	value <- x$get()
	result <- solve(value, ...)
	x$setSolveCache(result)
	return(result) 
}

