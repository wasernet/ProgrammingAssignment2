## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#
#
#
makeCacheMatrix <- function(mat = matrix()) {
#init the inverse to null
    inv <- NULL
    setmatrix <- function(y) {
        #set a new matrix ; escape his own env
        mat <<- y
        # new matix >> set inverse to null; escaping again
        inv <<- NULL
    }
    # display the embedded matix
    getmatrix <- function() mat
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = setmatrix, get = getmatrix, 
            setinverse = setinv, getinverse = getinv)
}


## Write a short comment describing this function
#
#
#
cacheSolve <- function(mat, ...) {
    ## Return a matrix that is the inverse of 'mat'
    inv <- mat$getinv()
    #return the cached value if not null
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    # retrieve the matrix
    data <- mat$get()
    # compute its inverse
    inv <- solve(data, ...)
    # caching the inverse
    x$setinv(inv)
    # diplay the 
    inv
}
