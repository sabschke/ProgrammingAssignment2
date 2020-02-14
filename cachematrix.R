## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly. Below are two functions that are used to create a special 
## object that stores a matrix and caches its inverse.



## The first function creates a list containing a function to:
## set the value of the matrix
## get the value of the matrix
## set the inverse of the matrix
## get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) i <<- inv
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}

## The following function calculates the inverse of the matrix 
## created with the above function. However, it first checks 
## to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the 
## computation. Otherwise, it calculates the inverse of the data 
## and sets the value of the inverse in the cache via the 
## setmean function.

cacheSolve <- function(x, ...) {
        
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i

}    

