## These functions allow for caching of a matrix inverse so it doesn't need ## to be calculated more than once

## This function creates a "matrix" object that caches the inverse of 
## the matrix

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <<- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(inv) inverse <<- inv 
        getInverse <- function() inverse
        list(set = set, get = get, setInverse = setInverse, 
             getInverse = getInverse)
}


## This function calculates the inverse of a "matrix" object, using its 
## cached inverse if it's already calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        if(!is.null(inverse)) {
                message("getting cached inverse")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data)
        x$setInverse(inverse)
        inverse
}
