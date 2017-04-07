## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix builds a matrix for cache its inverse

makeCacheMatrix <- function(x = matrix()) {

        inv = NULL
        set = function(y) {
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function
## cacheSolve computes the inverse of the matrix, directly from the cache if it has already been calculated and the matrix is the same

cacheSolve <- function(x, ...) {

        inv = x$getinv()
        if (!is.null(inv)){
                message("Get from the cache")
                return(inv)
        }
        
        mat.data = x$get()
        inv = solve(mat.data, ...)
        x$setinv(inv)
        return(inv)
	
}
