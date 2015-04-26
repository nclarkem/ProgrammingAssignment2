## Put comments here that give an overall description of what your
## functions do

## The function 'makeCacheMatrix' creates a special matrix, which is 
## really a list containing a function to return a list containing functions to
## 1- set the matrix
## 2- get the matrix
## 3- set the inverse
## 4- get the inverse
##  this list is used as the input to cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, 
         setinv = setinv, 
         getinv = getinv)
}


## This function computes the inverse of the matrix returned by makeCacheMatrix()
## 

cacheSolve <- function(x, ...) {
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    mat.data = x$get()
    inv = solve(mat.data, ...)
    
    x$setinv(inv)
    return(inv)
}
