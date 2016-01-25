
makeCacheMatrix <- function(x = matrix()) {  ##sets a vector, or a list of functions,
                                          ## that allows to set, get matrix and its inverse
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



cacheSolve <- function(x, ...) { ##checks whether an inverted matrix has already been calculated and
  ## stored in cache and returns the inverted matrix or calculates the inverted matrix if
  ## it has not been already calculated
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
