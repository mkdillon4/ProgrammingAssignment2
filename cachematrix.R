## These functions will cache a matrix and return the inverse of the matrix

## This function will create a special vector.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() {x}
    setInverse <- function(inverse) (inv <<- inverse)
    getInverse <- function() (inv)
    list(set = set = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special matrix.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}
