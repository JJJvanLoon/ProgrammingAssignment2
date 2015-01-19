## Hi Coursera!
## These functions save time by caching the inverse of a matrix
## preventing calculating the same inverse multiple times

## This function makes a cached version of a matrix. 
## This cache is capable to also save the inverse of the matrix, 
## when calculated with cacheSolve 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function solves the inverse of a matrix and stores that in the cache made by makeCacheMatrix.
## This function is also capable of comparing a control matrix, given in a seperate argument with the cached matrix.
## And notifies the user if the control matrix is different from the cached matrix.

cacheSolve <- function(x, c = "empty",...) {
        m <- x$getinv()
        if (identical(c,"empty") & !is.null(m)) {
                message("getting cached inverse matrix")
                return(m)
        } else if(identical(x$get(),c) & !is.null(m)) {
                message("matrix unchanged")
                message("getting cached inverse matrix")
                return(m)
        } else if (!identical(x$get(),c)& !identical(c,"empty")) {
                message("The given control matrix is not identical to the cached matrix.")
                message("Make a new cache with makeCacheMatrix.")
        } else {
                data <- x$get()
                m <- solve(data, ...)
                x$setinv(m)
                m
        }
}
