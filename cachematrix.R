## Programming exercise for R-Programming week#3
## Based on the functions example, the solution has to determine 
## if a matrix inverse has been done, if it has been done,
## then the calculation doesn't has to do it, and pull from cache
## otherwise, it has to be calculated

## basic functions for the matrix that we'll work on
## set the values of the matrix, get the values,
## calculate the "Solve" (matrix invertion)
## get the results of the invertion

makeCacheMatrix <- function(x = matrix()) {
        ix <- NULL
        set <- function(y) {
                x <<- y
                ix <<- NULL
        }
        get <- function() x
        setinv <- function(solve) ix <<- solve
        getinv <- function() ix
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ix <- x$getinv()
        if(!is.null(ix)) {
                message("getting cached data")
                return(ix)
        }
        data <- x$get()
        ix <- solve(data, ...)
        x$setinv(ix)
        ix
}
