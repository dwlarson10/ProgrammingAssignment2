## As a whole these functions take in a matrix and cache the inverse of the matrix
## This is an assignment for the r programming course on coursera. The functions below were modeled after the 
## example functions. 

## This function takes in a matrix of anysize. If the matrix is new it caches the matrix.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) inverse <<- solve
        getsolve <- function() inverse
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function calls searches for the cached matrix. If it is not there, it returns caches it and returns the inverse. If it is already cached, it returns the message "getting cached data" and returns the inverse. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getsolve()
        if(!is.null(inverse)) {
                message("getting cached data.")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data)
        x$setsolve(inverse)
        inverse
}


