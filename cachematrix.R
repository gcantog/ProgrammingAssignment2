## As part of assignment2, these functions compute the inverse of the
## special "matrix"


## makeCacheMatrix creates a special "Matrix", object that can 
## cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function()x
        setsolve <- function(inverse) s<<-solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cachesolve computes the inverse of the special "matrix" 
## returned by makeCacheMatrix

cachesolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached inverse")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}


x<-matrix(c(1,2,3,0,1,4,5,6,0),3,3)
anothermatrix<-makeCacheMatrix(x)
anotherMatrix$get()
cachesolve(anothermatrix)