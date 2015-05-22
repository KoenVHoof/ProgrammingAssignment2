
##This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
##set the value of the vector
    set <- function(y) {
        x <<- y;
        inverse <<- NULL;
    }
##get the value of the vector
    get <- function() return(x);
##set the value of the inverse
    setinv <- function(inv) inverse <<- inv;
##get the value of the inverse
    getinv <- function() return(inverse);
    return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
    inverse <- x$getinv()
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
    if(!is.null(inverse)) {
        message("Getting cached data...")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinv(inverse)
    return(inverse)
}
