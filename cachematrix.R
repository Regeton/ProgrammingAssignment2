## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(m = matrix()) {
        i <- NULL
        set <- function(y) {
                m <<- y
                i <<- NULL
        }
        get <- function() m
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...) 
        x$setinv(i)
        i
}

## Testing the makeCacheMatrix and cacheSolve functions

m1 <- matrix(1:9, nrow = 3, ncol = 3)
m2 <- matrix(runif(25, 0, 10), nrow = 5, ncol = 5)
mcm <- makeCacheMatrix(m1)
mcm$get()
mcm$set(m2)
mcm$get()
cacheSolve(mcm)
cacheSolve(mcm)
cacheSolve(mcm) %*% mcm$get()
