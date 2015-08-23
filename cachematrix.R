## The set of 2 functions contained in this script are aimed to cache and provide back the inverse
## of a matrix without having to recalculate it each time the inverse is needed. This also supposes
## that the matrix hasn't changed.

## This first function creates a special "matrix" object that can cache its inverse.
## The argument (input) of this makeCacheMatrix function  needs to be a square invertible matrix.

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) s <<- solve
    getinverse <- function() s
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}
y <- makeCacheMatrix(x)

## This second function computes the inverse of the special "matrix" returned by makeCacheMatrix
## above. If the inverse has already been calculated (and the matrix and its name have not
## changed), then the cachesolve retrieves the matrix inverse from the cache.

cacheSolve <- function(y) {
        ## Return a matrix that is the inverse of 'x'
        ## y should be the output of the above function make CacheMatrix function
    s <- x$getinverse()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setinverse(s)
    s
}