## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m     ## Return a matrix that is the inverse of 'x'
}

#l <- makeCacheMatrix()   # create cacheable matrix object
#testmatri <- rbind(c(1, -1/4), c(-1/4, 1)) #create a matrix for testing
#l$set(testmatri) #assign it to crrated object
#l$get(testmatri) #check its value
#cacheSolve(l) #create inverted matrix
#cacheSolve(l) #create again - should be getting it from cache
#cacheSolve(l)%*%testmatri #test that matrix is nverted - product should be an identitty matrix


# see benefit of caching on a bigger matrix
#l$set( matrix( rnorm( 1000000 ), 1000, 1000 ) )
#cacheSolve(l)
#cacheSolve(l) # much faster

