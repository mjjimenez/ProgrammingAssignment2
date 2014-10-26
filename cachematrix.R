## Matrix functions that have the ability to create a special matrix
## that can calculate and cache that matrix's inverse

## Create a matrix that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
    
        i <- NULL
        
        set <- function(y){
            x <<- y
            i <<- NULL
        }
        get <- function() x
        
        setInverse <- function(inverse) i <<- inverse
        getInverse <- function() i
        
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## Calculate the matrix inverse and store it in it's cache.

cacheSolve <- function(x, ...) {
    
        i <- x$getInverse()
        if(!is.null(i)) {
            message("getting cached data")
            return(i)
        }
        
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}
