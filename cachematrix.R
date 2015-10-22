## These functions store a matrix and its inverse and provides methods to 
## operate with them.

makeCacheMatrix <- function(x = matrix()) {
    ## This function stores the inverse of matrix 'x' as 'inv' and provides 
    ## functions to set and retrieve both matrices.
    
    # initialize 'inv' as an empty matrix
    inv <- matrix()
    
    # substitutes the stored matrix 'x' with a new matrix 'y'
    set <- function(y) {
        x <<- y
        inv <- matrix()
    }
    
    # retrieves de original matrix 'x'
    get <- function() {
        x
    }
    
    # stores the inverted matrix as 'inv'
    setinv <- function(inverted) {
        inv <<- inverted
    }
    
    # retrieves the inverted matrix
    getinv <- function() {
        inv
    }
}



cacheSolve <- function(x, ...) {
    ## This function finds the inverse of a matrix 'x' and stores both
    ## matrices using the previous function.
    
    # First things first, does the matrix have an inverse?
    if (det(x) = 0) {
        message("The matrix hasn't an inverse (determinant(M) = 0)")
        return(inv = matrix())
    }
    
    # retrieve the inverse matrix stored in cache
    inv <- x$getinv()
    
    # if the inverse has already been calculated, return it
    if (!is.na(x))
        message("retrieving cached data")
        return(inv)
    }
    
    # if not, get the matrix to be inverted, invert it and 
    # store the inverse using the cache function
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    
    # finally, return the inverse matrix
    inv
}
