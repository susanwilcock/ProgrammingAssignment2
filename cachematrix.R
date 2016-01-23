## Framework for storing a matrix and calculating and cacheing its inverse. 

## Returns a list of functions to set/get supplied matrix and to set/get its inverse. 
# We check that the the supplied parameter, x, is a matrix.  It is assumed that x is also 
# square invertible matrix. 
# NB: Lexical scoping utilized to store the matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
    
    # Holds inverse of stored matrix. 
    inv <- NULL
    
    # Functions created to set/get stored matrix.  
    set <- function(y) {
        # Our matrix is stored in parent environment
        x <<- y
        # New matrix may have a different inverse, so reset to NULL.
        inv <<- NULL
    }
    get <- function() x
    
    # Functions created to set/get inverse of stored matrix
    setSolve <- function(inverse) {
        # The inverse of our matrix is stored in the parent enviroment.
        inv <<- inverse
    }
    getSolve <- function() inv
    
    # Return four functions as a list with named elements.
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


## Tries to retrieve inverse matrix from cache (from x, the supplied list of functions)
# If the inverse is not cached, function will calculate it and cache it.
cacheSolve <- function(x, ...) {
    
    # Try to retrive from cache
    m <- x$getSolve()
    if(!is.null(m)) {
        message("retrieved cached data")
        return(m)
    }
    
    # Inverse wasn't cached, so retrieve original matrix, 
    # calculate inverse and store in cache
    data <- x$get()
    m <- solve(data, ...)
    x$setSolve(m)
    
    # Return inverted matrix
    m
}
