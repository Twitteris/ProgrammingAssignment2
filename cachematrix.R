## Put comments here that give an overall description of what your
## functions do

## This function takes as an input a matrix, then sets inve, the 
## soon to be inverse of a matrix, to NULL, then we apply
## set, get, setinve, getinve commands as in the
## example given with a vector and make a final list.

makeCacheMatrix <- function(x = matrix()) {
    inve <- NULL
    set <- function(y){
        x<<-y
        inve <<- NULL
    }
    get <- function()x
    setinve <- function(inverse) inve<<-inverse
    getinve <- function() inve
    list(set=set,get=get,setinve=setinve,getinve=getinve)
}


## This function takes in as an argument a matrix made by
## makeCacheMatrix, if the inverse is not NULL, it returns
## that non-NULL value. If it's not the case, we calculate
## the inverse of the matrix and output it.

cacheSolve <- function(x, ...) {
    inve<- x$getinve()
    if(!is.null(inve)){
        message("getting cached data")
        return(inve)
    }
    data <- x$get()
    inve <- solve(data,...)
    x$setinve(inve)
    inve
}
