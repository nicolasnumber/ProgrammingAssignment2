# Create a special matrix 
#
# Args:
#   x: a matrix
#    
# Returns a list containing the following functions
#  - set: set the value of the matrix
#  - get: get the value of the matrix
#  - setinverse: set the inverse of the matrix
#  - getinverse: get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() { return(x) }
    setinverse <- function(inverse) { inv <<- inverse }
    getinverse <- function() { return(inv) }
    return(list( 
        set = set, 
        get = get,
        setinverse = setinverse,
        getinverse = getinverse)
    )
}


# Calculate inverse of a special matrix
#
# The function checks if the inverse exists in the cache.
# If not, it calculates it and puts it in the cache.
#
# Args: 
#  x: a special matrix
#
# Returns:
#  The inverse of the matrix 
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    return(inv)
}
