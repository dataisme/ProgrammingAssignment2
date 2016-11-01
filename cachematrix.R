
## makeCacheMatrix creates a matrix whose inverse can be cached

## set -> sets the value of matrix
## get -> gets the value of the matrix
## setinverse -> sets the value of the inverse
## getinverse -> gets the value ofthe inverse

## the input value of inverse is stored in i and returned with getinverse function

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
        x <<- y
        i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve computes the inverse but before that
## it checks if it had already been solved and stored in cache
## in which case the value of inverse is directly retrieved from cache without any calculation

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
