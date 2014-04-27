## The following two functions makeCacheMatrix and cacheSolve  are used to cache the inverse of a matrix.


 ##The function makeCacheMatrix creates a special "matrix"    object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## The function cacheSolve calculates the inverse of the special "matrix" returned by above makeCacheMatrix function.

##If the inverse has already been calculated (and the matrix has not changed), then this function will retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}

