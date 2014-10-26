## These "makeCacheMatrix" and "cacheSolve" can be used to create a matrix
## and calculate and cache the inverse

## The "makeCacheMatrix"function creates a matrix that can also cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y) {
                x<<-y
                m<<-NULL
        }
        
        get<- function()x
        setinverse<- function(inverse)m <<- inverse
        getinverse <- function()m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        
}


## The "cacheSolve" function returns the inverse of the matrix created by the
## makeCachefunction. If the inverse has already been created, then the cached
## inverse is retrieved.If the inverse has not been created, then the inverse
## is calculated.

cacheSolve <- function(x, ...) {
        
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
                
        }
        
        data<- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        
}