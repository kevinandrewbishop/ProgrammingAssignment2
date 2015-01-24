## These functions create a special "matrix" object that can solve and cache its inverse to avoid
## repeatedly performing the computationally expensive task of inverting a matrix.

## This function creates a "matrix" than can cache its inverse.
## For example, to create a special "matrix" called foo, using some matrix M type:
## foo = makeCacheMatrix(M)

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Checks whether the "matrix" has already cached its inverse. If not, it solves and
## returns its inverse. If so it returns the cahsed inverse. Continuing the example above:
## inverse <- cacheSolve(foo)

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
