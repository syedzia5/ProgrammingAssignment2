## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The makeCacheMatrix creates a special matrix out of the input matrix.
## The matrix is a list of functions:
## 1. get()
##    gets the value special matrix which is a normal matrix
##
## 2. set(y)
##    sets the value of the special matrix to 'y'
## 
## 3. setinv(inv)
##    sets the internal cache variable(for storing inverse of matrix) to 'inv'
##
## 4. getinv()
##    gets the cached inverse

makeCacheMatrix <- function(x = matrix()) {
        Inv <- NULL
        set <- function(y) {
                x <<- y
                Inv <<- NULL
        }
        get <- function() x
        setinv <- function(inv) Inv <<- inv
        getinv <- function() Inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function

## The cacheSolve function returns the inverse of the special 
## matrix created by makeCacheMatrix.
##
## It first checks if the inverse exists in the cache:
## 		if the inverse exists in the cache then returns the cached inverse
##		else calculates the inverse, caches it and returns the calculated inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("Inverse already calculated and cached, returning inverse from cache")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
