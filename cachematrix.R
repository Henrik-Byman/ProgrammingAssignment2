## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates an environment containing a variable for a matrix and
## its inverse value, as well as getters and setter for both variables. 
## set function resets the matrix with given value and resets its inverse to NULL
## get function returns the stored matrix
## setInverse function set the value for the inverse variable
## getInverse function returns the inverse variable

makeCacheMatrix <- function(x = matrix()) {

    inverse <- NULL
    
    set <- function(matrix){
        x <<- matrix
        inverse <<- NULL
    }
    
    get <- function() x
    
    setInverse <- function(inv) inverse <<- inv
    
    getInverse <- function() inverse
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## This function attempts to return the cached value of the inverse matrix
## stored inside the makeCacheMatrix environment. If the inverse is null, the
## function attempts to recalculate the inverse value and then return it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    inverse <- x$getInverse()
    
    if(!is.null(inverse)){
        message("Retrieving the cached inverse matrix.")
        return(inverse)
    } else {
        matrix <- x$get()
        message("Calculating the inverse matrix.")
        inverse <- solve(matrix, ...)
        x$setInverse(inverse)
        inverse
    }
}
