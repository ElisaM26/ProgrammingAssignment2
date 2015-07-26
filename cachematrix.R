## The following pair of functions caches the inverse of a matrix
## so that it does not have to be computed repeatedly.


## The makeCacheMatrix function creates a matrix object that can
## store a numeric matrix ('x') and cache its inverse. It includes
## functions that: set the values of the matrix 'x', get the values
## of the matrix 'x', set the values of the inverse matrix 'i', and
## get the values of the inverse matrix 'i'

makeCacheMatrix <- function(x = matrix()) {
        i <- matrix()
        set <- function(y) {
                x <<- y
                i <<- matrix()
        }
        get <- function () x
        setinverse <- function(inver) i <<- inver
        getinverse <- function() i
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## The cacheSolve function computes the inverse of the matrix
## that is returned by the function makeCacheMatrix. If the 
## inverse has previously been calculated, and the original matrix
## has not been changed, the cacheSolve function retrieves the
## inverse from the cache rather than computing it again.

cacheSolve <- function(x) {
        ## Return a matrix 'i' that is the inverse of the matrix
        ## stored in 'x'
        i <- x$getinverse()
        if(!any(is.na(i))) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
}