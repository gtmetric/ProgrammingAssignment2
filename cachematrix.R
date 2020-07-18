## These functions aim to cache the inverse of matrices since calculating the
## inverse is costly.

## This function is to create a special matrix that can cache a calculated
## inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function()
        x
    
    setinv <- function(inverse)
        inv <<- inverse
    
    getinv <- function()
        inv
    
    list(
        set = set,
        get = get,
        setinv = setinv,
        getinv = getinv
    )
}


## This function is to check for the cached inverse of a matrix before
## calculating the matrix.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    data <- x$get()
    
    if(!is.null(inv)) {
        message('Returning cached data')
        return(inv)
    }
    
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
