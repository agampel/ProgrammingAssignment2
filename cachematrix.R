## These functions together, generate a matrix that can be cached,
## check if the inverse of a matrix
## has been computed, return the result from the cache if it exists there
## or computes the inverse of a matrix and caches the result if not.


## This function creates a special matrix
makeCacheMatrix <- function(x=matrix()){
        m<- NULL
        
        ## set value of the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## get the value of the matrix
        get <- function() x
        
        ## set the value of the matrix
        setinv <- function(solve) m <<- solve
        
        ## get the value of the matrix
        getinv <- function() m
        
        list(set = set, get = get,
             setinv = setinv, 
             getinv = getinv)
}

## function to check it a cached matrix containing the inverse of
## the input matrix exists.
## if it does, return the cached result.
## if it doesn't, compute the inverse, cache it and return the result.
cachesolve <- function(x, ...) {
        
        ## get the calculated inverse of input matrix
        m <- x$getinv()
        
        ## check if the matrix has already been calculated.
        if(!is.null(m)) {
                ## if the matrix has been calculated, 
                ## send message and return matrix
                message("getting cached data")
                return(m)
        }
        ## if the matrix has not been calculated, get the value of the matrix
        data <- x$get()
        
        ## calculate the inverse of the matrix
        m <- solve(data, ...)
        
        ## set the value of the inverse of the matrix
        x$setinv(m)
        
        ## return the inverse of the matrix
        m
}
