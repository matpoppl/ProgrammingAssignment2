
makeCacheMatrix <- function(x = matrix()) {
    # initialize inverse matrix
    i <- NULL
    
    # set new matrix method
    set <- function(y) {
        # overwrite existing matrix
        x <<- y
        # reset inverse matrix
        i <<- NULL
    }
    
    # get matrix method
    get <- function() x
    
    # set new inverse matrix method
    setinverse <- function(inverse) i <<- inverse
    
    # get inverse matrix method
    getinverse <- function() i
    
    # set externally available methods
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
    
    # get inverse matrix from memory
    i <- x$getinverse()
    
    # is inverse matrix already exists
    if(! is.null(i)) {
        # debug message
        message("getting cached data")
        
        # return cached inverse matrix
        return(i)
    }
    
    # get matrix
    data <- x$get()
    
    # calculate inverse matrix
    i <- solve(data, ...)
    
    # store in memory
    x$setinverse(i)
    
    # return inverse matrix
    i
}
