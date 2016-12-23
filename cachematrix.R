## Assignment 2 R Programming
## Joanna Chaney
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x=matrix()){
        m <- NULL
        set<-function(y) {
        x<<-y
        m<<-NULL
        }
        
        get <-function()x
        setinverse <- function(solve) m<<-solve
        getinverse<-function() m 
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) #returns a list of functions
       
}

## This function computes the inverse of the special "matrix" returned by  
## makeCacheMatrix  above. If the inverse has already been calculated 
## (and the matrix has not changed), then  cacheSolve  should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()       ##get the inverted matrix from makeCacheMatrix
        if(!is.null(m)) {         ##check to see if the inversed matrix is already cached
        message("getting cached data")
        return(m)                 ##return the inverted matrix
        }
        data <- x$get()           ##else get the matrix
        m <- solve(data, ...)     ##and invert it
        x$setinverse(m)           ##cache the inverse
        m                         ##return the inverted matrix
}
