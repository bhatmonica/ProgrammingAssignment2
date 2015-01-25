## R- Programming Assignment 2
## The functions below are designed to optimize potentially time-consuming computations. 
## For example, creating inverse of a matrix. The functions incorporate scoping rules of the R language 
## to preserve state inside of an R object. 


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {                            # Modify existing matrix
        x <<- y
        i <<- NULL
    }
    get <- function() x                              # Returns matrix
    setinverse <- function(inverse) i <<- inverse    
    getinverse <- function() i                       # Returns matrix inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}