## Put comments here that give an overall description of what your
## functions do

## This function takes a matrix and returns a list containing the matrix and additional parameters

makeCacheMatrix <- function(x = matrix()) {
    im <- NULL
    set <- function(y) {
        x <<- y
        im <<- NULL
    }
    get <- function() x
    setinvmatrix <- function(invmtx) im <<- invmtx
    getinvmatrix <- function() im
    list(set = set, get = get, setinvmatrix = setinvmatrix, getinvmatrix = getinvmatrix)
}


## This function checks whether the inverse is already cached for the matrix.  If not, it returns
## the matrix.  If it exists, it prints a message saying it is pulling from cache and displays.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    im <- x$getinvmatrix()
    if(!is.null(im)) {
        message("Getting cached data")
        return(im)
    }
    data <- x$get()
    im <- solve(data, ...)
    x$setinvmatrix(im)
    im
}
