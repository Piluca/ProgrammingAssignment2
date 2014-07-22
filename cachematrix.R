## The purpose of the following functions is to avoid repeated computations of
## the inverse of a matrix that could be time consuming. This is done by means 
## of a cache that stores inverse values already calculated, so that they are
## available if they are needed again, avoiding recomputations.

## makeCacheMatrix creates an special "matrix" object from the given matrix.
## This special object is able to store the value of the matrix given and
## to cache its inverse, using the methods 'set' and 'setinverse', respectively.
## Additionally, it can return both values on request by using the methods
## 'get' and 'getinverse'.

makeCacheMatrix <- function(x = matrix()) {
    
    ## Initialize inverse matrix value
    i <- NULL
    
    ## function to set the value of the matrix
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    ## function to get the value of the matrix
    get <- function() x
    
    ## function to set the value of the inverse of the matrix
    setinverse <- function(inverse) i <<- inverse
    
    ## function to get the value of the inverse of the matrix
    getinverse <- function() i
    
    ## list of all the functions available from this function.
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## cacheSolve returns a matrix representing the inverse of the matrix provided as an input.
## The input is a special matrix created with the function makeCacheMatrix. To obtain the
## inverse, cacheSolve checks if it has been previously calculated by means of a cache. If
## so, returns the value stored in the cache, avoiding calculations. Otherwise, it calculates
## the inverse and store it in the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## Try to find the cached value of the inverse of matrix 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        ## If the value is found in the cache, return the cached value.
        message("getting cached data")
        return(i)
    }
    
    ## If the value is not in the cache, calculate it.
    data <- x$get()
    i <- solve(data, ...)
    
    ## Cache the value obtained
    x$setinverse(i)
    i
}
