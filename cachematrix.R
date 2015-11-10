## The following functions aim to cache potentially time-consuming computations.
## For a very large matrix, it may take too long to compute the inverse, especially if it has to be
## computed repeatedly (e.g. in a loop). If the contents of a matrix are not changing, it may make 
## sense to cache the value of the inverse so that when we need it again, it can be looked up in the
## cache rather than recomputed.

## makeCacheMatrix creates a special "matrix" which is a list containing functions to set the value
## of the matrix ("set"), get the value of the matrix ("get"), set the value of the inverse
## ("setinverse") and get the value of the inverse ("getinverse")

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated, then the cacheSolve should retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    ## If the inverse is not NULL, then the cached inverse is retrieved
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    ## If the inverse is NULL, then the inverse is solved for and returned
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}
