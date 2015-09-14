## Functions to compute the inverse of a matrix and cache the results

## Creates an object with space for caching an inverse of the matrix data passed in
makeCacheMatrix <- function(x = matrix()) {
    # Init cache to NULL
    i <- NULL
    
    # Set data; reset cache to NULL
    set <- function(y) {
            x <<- y
            i <<- NULL
    }
    
    # Gets data
    get <- function() x
    
    # Sets cache
    setinv <- function(inv) i <<- inv
    
    # Gets cache
    getinv <- function() i
    
    # Public methods
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Computes the inverse of the data passed in to 'makeCacheMatrix"
cacheSolve <- function(x, ...) {
    # Retrieve cache
    i <- x$getinv()
    
    # Return the cache if it is not NULL
    if(!is.null(i)) {
            message("getting cached data")
            return(i)
    }
    
    # Otherwise, retrieve the data
    data <- x$get()
    
    # Compute the inverse
    i <- solve(data, ...)
    
    # Set the cache
    x$setinv(i)
    
    # Return the inverse
    i
}