## Programming Assignment 2


## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {      ## a function setting the variables in the environment
        x <<- y               ## making "y" the matrix you supplied and 
        m <<- NULL            ## emtying "m" 
    }
    get <- function() x                    ## "get" means "show me x"
    setinv <- function(solve) m <<- solve  ## "setinv" will store the inversion result ("solve" being the command for inverting the matrix)
    getinv <- function() m                 ## "getinv" will show the inverted matrix (if it is calculated)
    list(set = set, get = get,             ##list of variables that can be used as commands in the form "x$variable"
         setinv = setinv,
         getinv = getinv)
}


cacheSolve <- function(x, ...) {
    m <- x$getinv()                     ## make "m" the value of the list object "getinv" from the list created in makeCacheMatrix 
    if(!is.null(m)) {                   ## if m is not empty, show it's value 
        message("getting cached data")
        return(m)
    }
    data <- x$get()                     ## fill "data" with value from list object "get" from the list created in makeCacheMatrix
    m <- solve(data, ...)               ## as m is not yet calculated, calculate it (inversion of matrix)
    x$setinv(m)                         ## store it for future collection from cache
    m                                   ## Return result (this time not from cache)
}


## and here is the set of commands you can use to test hte functions, just removew the "#" from the start of the line:

#l <- makeCacheMatrix()                      ## create cacheable matrix object
#testmatri <- rbind(c(1, -1/4), c(-1/4, 1))  ## create a matrix for testing
#l$set(testmatri)                            ## assign it to a created object "l"
#l$get()                                     ## check its value so you know it's stored
#cacheSolve(l)                               ## invert the matrix
#cacheSolve(l)                               ## invert it again - should be getting it from cache this time
#cacheSolve(l)%*%testmatri                   ## test that matrix is nverted - product should be an identitty matrix


## see the benefit of caching on a bigger matrix

#l$set(matrix(rnorm(1000000), 1000, 1000))         ## create a bigger matrix
#cacheSolve(l)                                     ## invert it first time (notice the amount of time it takes)
#cacheSolve(l)                                     ## second time will be much faster - specially if you have an old computer

