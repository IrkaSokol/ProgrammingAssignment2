## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {  #sets a vector, or a list of functions,
                                          # that allows to set, get matrix and its inverse
        invx <- NULL
        set <- function(y) {
                x <<- y
                invx <<- NULL
        }
        get <- function() x
        setinvmatrix <- function(invertmatrix) invx <<- invertmatrix
        getinvmatrix <- function() invx
        list(set = set, get = get,
             setinvmatrix = setinvmatrix,
             getinvmatrix = getinvmatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {   #checks an invert matrix in cache and returns or calculates the inverse
        invx <- x$getinvmatrix()
        if(!is.null(invx)) {
                message("getting cached data")
                return(invx)
        }
        data <- x$get()
        invx <- solve(data)
        x$setinvmatrix(invx)
        invx
                
        ## Return a matrix that is the inverse of 'x'
}
