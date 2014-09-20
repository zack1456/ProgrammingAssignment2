## makeCacheMatrix is a helper function that creates a list containing a function that gets and sets values of a matrix
## as well as the (cached) inverse value of that matrix
##
## cacheSolve uses makeCacheMatrix to solve for the inverse of a matrix, using the cached value if available or computing
## the inverse if not

## makeCacheMatrix initializes a variable to store the matrix and a null value for the inverse, along with functions to
## compute and cache the inverse when called

makeCacheMatrix <- function(x = matrix()) {
        # set default value for inverse, uncached
        inv <- NULL
        # sets up pre-computed list with matrix stored and NULL for inverse since we haven't computed anything
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        # function to retreive matrix data
        get <- function() x
        # function to solve for inverse and store in cache
        setinverse <- function(solve) inv <<- solve
        # function to retrieve inverse 
        getinverse <- function() inv
        # function to set up internal storage of data
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        # see if inverse is available
        inv <- x$getinverse()
        # if inverse is not null, return cached value
        if(!is.null(inv)) {
                message("getting cached inverse data")
                return(inv)
        }
        # if inverse was not available, get stored matrix
        data <- x$get()
        # find inverse value of stored matrix
        inv <- solve(data, ...)
        # store cached inverse
        x$setinverse(inv)
        # return inverse matrix
        inv
}
