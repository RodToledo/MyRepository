## These two functions that are used to create a special object that 
## stores a matrix and cache's its inverse. The matrix has to be invertible.

## Creates a list containing functions to: set/get matrix and set/get inverse. 

makeCacheMatrix<- function(x=matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(Inverse) inv <<- Inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Calculates de inverse of the list created in the above function. 
## If its in the cache then uses that inverse and skips the computation.

cacheSolve<- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
