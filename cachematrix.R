## These functions cache the inverse of a matrix. The cached inverted matrix is stored in 
## makeCacheMatrix's environment rather than in the global environment.

## This function defines 4 functions: set, get, setinverse, and getinverse. In doing so it 
## creates an environment isolated from the global environment. In this isolated environment, 
## values from cacheSolve can be cached.

makeCacheMatrix<-function(x=matrix()){
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function checks if the inverse of a matrix has already been calculated and cached. If so, 
## it retreives the cached inverse. If the inverse has not yet been stored, this function inverts a 
## matrix, then sends the value of the inverse to makeCacheMatrix's environment to be stored.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data<-x$get()
        m<-solve(data)
        x$setinverse(m)
        m
}