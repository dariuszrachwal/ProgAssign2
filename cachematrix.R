## We will use this functions to cache an inverted matrix to the given
## one

## This function creates "matrix" object (being a list in fact), which
## can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" 
## returned by previous function

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    if(det(data)==0){
        message("Matrix must be invertible!")
    }
    else{
        m <- solve(data, ...)
        x$setsolve(m)
        m
    }
}
