## Writing 2 functions to cache the matrix and find the inverse of the matrix
## makeCacheMatrix function creates a copy of recent matrix and cacheSolve finds the inverse of the matrix
## cacheSolve finds inverse only after checking with the cache if its already run and available else gets result from cache

## Example showing the implementation of the function
## m<- makeCacheMatrix(matrix(c(3,2,1,4),c(2,2)))
## cacheSolve(m)
##    [,1] [,2]
##[1,]  0.4 -0.1
##[2,] -0.2  0.3
## Repeating the same cacheSolve function on same value of m to confirm it takes from cache when its run before.
## cacheSolve(m)
##getting cached data 
##     [,1] [,2]
##[1,]  0.4 -0.1
##[2,] -0.2  0.3
## when run again it took the cached data and hence displaying "getting cached data" 


## Creating a special matrix of n rows and m columns assigned by user to get its inverse 
## Below function contains below variables
## SET  - set the value of the matrix
## GET  - get the value of the matrix
## SETINVERSE  - set the value of the inverse matrix
## GETINVERSE  - get the value of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) m <<- inv
    getinverse <- function() m
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
}


## Calculate the inverse of the special "matrix" created with the above
## function, reusing cached result if it is available

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(i, ...)
    x$setinverse(m)
    m
}
