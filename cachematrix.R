## makeCacheMatrix is a helper function returning a list of setters and getters for matrix and its reverse matrix
## cacheSolve returns cached reverse matrix if available otherwise solves and returns the inversion

## This function returns a list of functions (set, get, setreverse, getreverse) that store and return the matrix
## given as input parameter and its inversion.

makeCacheMatrix <- function(x = matrix()) {
    r <- NULL
    set <- function(y) {
        x <<- y
        r <<- NULL
    }
    get <- function()
        x
    setreverse <- function(reversed)
        r <<- reversed
    getreverse <- function()
        r
    list(
        set = set, get = get,
        setreverse = setreverse,
        getreverse = getreverse
    )
}


## This function checks wether we already have computed inversion of the input matrix.
## If yes, it simply returns the result, otherwise it computes it, stores it for further use and then returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    r <- x$getreverse()
    if (!is.null(r)) {
        message("getting cached data")
        return(r)
    }
    data <- x$get()
    r <- solve(data, ...)
    x$setreverse(r)
    r
}
