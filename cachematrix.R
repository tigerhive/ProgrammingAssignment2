# Matrix inversion is usually a costly computation and there may be some 
# benefit to caching the inverse of a matrix rather than compute it repeatedly.
# Together, this pair of functions allow the inverse of a matrix to be 
# solved just once and cached.  Subsequent calls to get the inverse will 
# retrieve the result from the cache.
#
# Example usage
# =============
#
# Setting the matrix object...
# m<-makeCacheMatrix( matrix(3:6,nrow=2, ncol=2) )
#
# Retrievinhg the matrix object...
# m$get()
#
# Getting the inverse...
# cacheSolve(m)

# makeCacheMatrix
# ===============
# This function creates a special "matrix" object that can cache its inverse.
# It assumes that the matrix supplied is always invertible.

makeCacheMatrix <- function(matrixIn=matrix()) {

    # Example usage
    # =============
    # Set matrix at creation time
    # m<-makeCacheMatrix( matrix(3:6,nrow=2, ncol=2) )
    
    # Create NULL matrix and set afterwards
    # m<-makeCacheMatrix( )
    # m$set(matrix(4:7,nrow=2, ncol=2))
    
    # Set this object's matrix to the input object
    # Set the inverse matrix to NULL
    this.matrix<-matrixIn
    inverse.matrix <- NULL
    
    # This function resets the object's matrix.
    # Set the inverse matrix to NULL at the same time, otherwise
    # it would be the wrong value for the new matrix.
    # <<- indicates that the values are coming from the parent
    # environment.
    reset <- function(y) {
        this.matrix <<- y
        inverse.matrix <<- NULL
    }
    
    # It's possible to call the function  without initialising
    # the matrix (i.e "m<-makeCacheMatrix( )") - in which case 
    # this.matrix is NULL.  Display a warning message for this 
    # case.
    get<-function() {
        if(is.null(this.matrix)) {
            message("Not yet defined")
        }
        this.matrix
    }
    
    # Set the inverse matrix to the value provided to the setinverse 
    # function - i.e. m$setinverse(THIS VALUE)
    setinverse <- function(varIn) inverse.matrix <<- varIn
    
    # Function to return inverse.matrix
    getinverse <- function() inverse.matrix
    
    # Return the list of functions.
    list(set=reset,
         get=get,
         setinverse=setinverse,
         getinverse=getinverse)
}

# cacheSolve
# ==========
# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and 
# the matrix has not changed), then the cachesolve should retrieve the 
# inverse from the cache.

cacheSolve <- function(matrixIn, ...) {

    inverse <- matrixIn$getinverse()
    # If the return matrix is not NULL, then
    # use it and finish.
    if(!is.null(inverse)) {
        message("Getting cached data")
        return(inverse)
    }
    # Set the inverse matrix as the input matrix.
    inverse <- solve(matrixIn$get(), ...)
    matrixIn$setinverse(inverse)
    # Return the inverse matrix.
    inverse
}
