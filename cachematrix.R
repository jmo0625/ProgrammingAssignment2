## these functions work together to caches the inverse of the intended matrix


## this function creates an matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInv <- function(inverse) {
        i <<- inverse
    }
    getInv <- function() i
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## This function finds the inverse of the matrix

cacheSolve <- function(x, ...) {
    i <- x$getInv()
    
    if( !is.null(i) ) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data,...)
    x$setInv(i)
    i
}
