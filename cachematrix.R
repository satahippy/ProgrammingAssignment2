## Functions for cacheable calculating the inverse matrix

## Returns the special 'matrix', which has ability to cache result of inversion

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    get <- function() {
        x
    }
    
    setinverse <- function(inverse) {
        i <<- inverse
    }
    
    getinverse <- function() {
        i
    }
    
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
}



## Inverse special 'matrix', writes result in cache, and returns result
## If cache is not empty, then the cached result will be returned

cacheSolve <- function(x, ...) {
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
