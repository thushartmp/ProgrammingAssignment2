## The following functions find the inverse of matrices and cachce it. The inverse is calculated and cached in makeCacheMatrix and  
## it will be used in cacheSolve where calculation is done if the value has not been previously cached.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)	

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 	i <- x$getinverse()
        	if(!is.null(i)) {
        	        message("getting cached data")
        	        return(i)
        	}
        	data <- x$get()
        	i <- solve(data, ...)
        	x$setinverse(i)
        	i
}
