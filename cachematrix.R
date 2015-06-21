## A pair of functions which cache the inverse of a matrix

##Creating a special matrix object that can cache its inverse 

makeCacheMatrix <- function (mtrx=matrix()) {
    inverse  <- NULL
    setting <- function(x) {
        mtrx <<- x ;
        inverse <<- NULL ;
    }
    getting <- function()return(mtrx);
    setinverse <- function(inv) inverse <<- inv;
    getinverse <- function()return(inverse);
    return(list(setting = setting, getting = getting, setinverse = setinverse, getinverse = getinverse))
}
  
##Function that computes the inverse of the special matrix 

cacheSolve <- function(mtrx,...) {
    inverse <- mtrx$getinverse()
    if(!is.null(inverse)){
        message("Getting cached data...")
        return(inverse)
    }
    data <- mtrx$getting()
    inverse <- solve(data,...)
    mtrx$setinverse(inverse)
    return(inverse)
}
