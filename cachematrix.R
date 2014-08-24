## Calculating the inverse of a matrix can take some time, so it is
## conveniente to cache the result of the inversion of the matrix to
## avoid calculating it over and over again. makeCacheMatrix makes a 
## list containing functions to save the inverse of a matrix.
## cacheSolve takes the vector created by makeCacheMatrix and returns 
## the inverse of the matrix by getting the cached datda or solving the 
## matrix

## makeCacheMatrix receives a square matrix and creates a list of
## functions for calculating its inverse and cacheing it.

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL
        set <- function(y = matrix) {
                x <<- y
                inv <<- NULL
        }
        get <- function()x 
        setinv <- function(res) inv <<- res
        getinv <- function()inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}
## cacheSolve receives the list of functions created by makeCacheMatrix
## and verifies if the inverse has already been calculated and gets it or
## otherwise calculates it. Either way, it returns the inverse matrix of
## the squared matrix input into makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

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
