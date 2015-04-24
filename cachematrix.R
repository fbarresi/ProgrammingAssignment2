## These functions create a special object that store a matrix and cache its inverse
## 

## This function create a special matrix
## which is really a list containing functions to
## set and get the values of the matrix
## and to set and get the values of the inverse of the matrix

makeCacheMatrix <- function(m = matrix()) {
	mI <- NULL
    set <- function(x) {
        m <<- x
        mI <<- NULL
    }
    get <- function() m
    setinverse <- function(inverse) mI <<- inverse
    getinverse <- function() mI
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function retrieves the inverse  
## of the matrix stored in a 'makeCacheMatrix' object.
## If it was alredy calculated, this function reads the cache of the object
## otherwise this function calculates the inverse matrix 
## and writes it in the cache of 'makeCacheMatrix'

cacheSolve <- function(m, ...) {
    mI <- m$getinverse()
    if(!is.null(mI)) {
        message("getting cached data")
        return(mI)
    }
    data <- m$get()
    mI <- solve(data, ...)
    m$setinverse(mI)
    mI
}
