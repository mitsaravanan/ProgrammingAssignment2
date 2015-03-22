# Matrix inversion is usually a costly computation and there may be some 
# benefit to caching the inverse of a matrix rather than computing it repeatedly
# The following two fuctions will help with the creation and caching of the inverse of a Invertible(or Non-singular) Matrix.

# Note: Not all square matrices have inverses. A square matrix which has an inverse is called invertible or nonsingular, 
# and a square matrix without an inverse is called noninvertible or singular.


# makeCacheMatrix function creates a special "matrix" object that can cache its inverse. This function is can
# 1. set a brand new matrix.
# 2. get the matrix we created above.
# 3. set the inverse of the matrix.
# 4. get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        invmatrix <- NULL
        set <- function(y) {
                x <<- y
                invmatrix <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) invmatrix <<- inverse
        getinverse <- function() invmatrix
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# cacheSolve function computes the inverse of the Invertible matrix returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then cacheSolve should retrieve the inverse from the cache of makeCacheMatrix.

cacheSolve <- function(x, ...) {
        invmatrix <- x$getinverse()
        if(!is.null(invmatrix)) {
                message("getting inverse of matrix from cache.")
                return(invmatrix)
        }
        inputmatrix <- x$get()
        message("creating inverse of matrix for the first time to store it in cache.")
        inverse <- solve(inputmatrix)
        x$setinverse(inverse)
        inverse
}
