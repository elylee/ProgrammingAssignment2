## Utility functions for matrix and the inverse of a matrix

## Creates a special object that stores a matrix and caches its inverse
## Returns a list containing functions to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
    mInv <- NULL
    
    set <- function(y){
        x <<- y
        mInv <<- NULL
    }
    
    get <- function() x
    
    setMatrixInverse <- function (mInverse) mInv <<- mInverse
    
    getMatrixInverse <- function () mInv
    
    list(set = set, get = get, setMatrixInverse = setMatrixInverse, getMatrixInverse = getMatrixInverse)
}


## Returns a matrix that is the inverse of 'x'
## If the inverse of the matrix 'x' has been previously cacluated and cached, the cached value is returned.

cacheSolve <- function(x, ...) {
    
    ## see if a cached matrix inverse already exists, if so, return the cached value
    mInv <- x$getMatrixInverse()
    if(!is.null((mInv))){
        message("getting cached matrix inverse")
        return(mInv)
    }
    
    ## otherwise, caculates the inverse of the matrix and cache the result
    matrix <- x$get()
    mInv <- solve(matrix, ...)
    x$setMatrixInverse(mInv)
    mInv
}
