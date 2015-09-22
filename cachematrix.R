## Put comments here that give an overall description of what your
## functions do

## Function that defines a cacheMatrix. This object has a get and set methods
## for the matrix and a get and set methods for the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    cmatrix <- NULL
    set <- function(y) {
        x <<- y
        cmatrix <<- NULL
    }
    get <- function() x
    setinversematrix <- function(imatrix) cmatrix <<- imatrix
    getinversematrix <- function() cmatrix
    list(set = set, get = get,
         setinversematrix = setinversematrix,
         getinversematrix = getinversematrix)
}


## Function that computes the inverse of a matrix wrapped in a matrixCache object
## If the matrixCache has computed the inverse it is returned otherwise it is computed
## and set it in the matrixCache object

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinversematrix()
    if(!is.null(m)) {
        message("Getting inverse matrix from cache")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinversematrix(m)
    m
}
