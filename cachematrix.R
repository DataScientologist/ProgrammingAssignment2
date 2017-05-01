## References:
## https://www.mathsisfun.com/algebra/matrix-inverse.html
## https://www.mathsisfun.com/algebra/matrix-introduction.html

## Creates a cached matrix for later evalution.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) i <<- inverse
        getInverse <- function() i
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Checks if mean exists or gets the mean

cacheSolve <- function(x, ...) {
         ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}
