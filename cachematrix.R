## The following functions are designed to cache the inverse of a matrix rather
## than computing it repeatedly.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL ## Set the cache
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        } ## Set the matrix
        
        get <- function() x ## Get the matrix
        
        setInverse <- function(inverse) cache  <<- inverse 
        ## Set the inverse of the matrix
        
        getInverse <- function() cache ## Get the inverse of the matrix
        
        list(set = set, get = get, setInverse = setInverse, getInverse = 
                     getInverse) ## Return a list
}


## cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        } ## Return the inverse if it is already cached
        
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m ## Determine the inverse and return it if it is not cached
}
