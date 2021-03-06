## There functions work together to provide the ability to calculate the 
## inverse of a square matrix.  The matrix and its inverse are cached to 
## provide faster retrieval of the inverse without needing to re-process the 
## matrix.


## This function can store a matrix and the inverse of the matrix

makeCacheMatrix <- function(m = matrix()) {
    i <- NULL
    set <- function(y) {
        m <<- y
        i <<- NULL
    }
    get <- function() m
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of a matrix, and it uses the 
## makeCacheMatrix function to store and retrieve the matix and its inverse for 
## faster processing.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
