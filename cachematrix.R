## Functions to create and use special versions of matricies 
## that can cache certain properties.
## Currently supported: inverse.

## Creates the special matrix from an existing one

makeCacheMatrix <- function(x = matrix()) {
        cachedInverse <- NULL
        set <- function(y) {
                x <<- y
                cachedInverse <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) cachedInverse <<- inverse
        getInverse <- function() cachedInverse
        list(set = set, get = get, 
             setInverse = setInverse,
             getInverse = getInverse)
}


## Returns the inverse of this matrix. 
## When called for the first time, it calculates the inverse and caches it.
## Immedialty returns the cached value on all subsequent calls.

cacheSolve <- function(x, ...) {
        inverse <- x$getInverse()
        if(!is.null(inverse)) {
                message("getting cached inverse")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setInverse(inverse)
        inverse
}
