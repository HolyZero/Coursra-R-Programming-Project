## Cache the inverse of a matrix to save run time.

## Creates a special "matrix" which has four functions. 
## Set the value of mx, get the value of mx, set the inv
## of mx, and get the inv of mx.
makeCacheMatrix <- function(x = matrix()) {
    m  <-  NULL
    set  <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv  <- function(inv) m <<- inv
    getinv  <- function() m
    list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}

## Computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If inverse has already been calculated
## then cacheSolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached inverse")
        return(m)
    }
    mtx <- x$get()
    m  <- solve(mtx,...)
    x$setinv(m)
    m
}
