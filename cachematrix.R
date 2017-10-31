## The functions below creates an inverse of a matrix and caches the data
## The functions are makeCacheMatrix and cacheSolve

## This function returns a function that sets and gets the cache

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}


## This function computes the inverse of a matrix if its not already computed
## If the inverse is already computed (from previous execution), then the inverse
## is returned from the cache
cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}

# Execution and sample working. You can replace A with any matrix
A = matrix( c(1,2,3,0,1,4,5,6,0), nrow=3, ncol=3, byrow = TRUE)
A
matA <- makeCacheMatrix(A)
matA
cacheSolve(matA)
# Note the second time cacheSolve is called the data should be retrieved from
# the cache instead of computing the inverse
cacheSolve(matA)