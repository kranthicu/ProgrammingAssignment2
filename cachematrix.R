## Goal of the functions is to cache the inverse of the matrix over repeatedly computing the inverse.

## makeCacheMatrix caches the inverse of matrix.

makeCacheMatrix <- function(x = matrix()) {
        i <- null
        set <- function(y){
                x <<- y
                i <<- null        
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get, setinv = setinv, getinv = getinv)
                                        
}


## cacheSolve computes the inverse of the matrix retruned by makeCacheMatrix. If the inverse is
## already calculated then its returns the cached inverse value
cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)){
                message("getting cached data")
                return(i)        
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
