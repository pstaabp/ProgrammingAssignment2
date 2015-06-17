## These are functions that store a matrix that will cache it inverse. 

## To use 
#  1) create a cacheMatrix like  cm<-makeCacheMatrix()
#  2) store a matrix in the cm object:  cm$set(matrix( rnorm(10*10,mean=0,sd=1), 10, 10))
#  3) then use cacheSolve to find the inverse: cacheSolve(cm)

# Note: the first time cacheSolve is run, it calculates the inverse and caches it
# Other times, it looks up the inverse matrix already calculated and stored. 

## This function creates a way to store a matrix that will cache its verse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) m <<- inv
    getinverse <- function() m
    list(set = set, get = get, getinverse = getinverse, setinverse=setinverse)
}


## This function will take a variable created with the makeCacheMatrix function
## and if the inverse has been cached, it will return that.  
## otherwise it will calculate it. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
