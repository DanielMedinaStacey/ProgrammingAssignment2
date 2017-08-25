## R PROGRAMMING - PROGRAMMING ASSIGNMENT 2

## These functions provide a way to calculate the inverse of a matrix and store
## it in a cache for subsequent use. This may save some time for very large 
## matrices or on slow machines.

## CREATE CACHED MATRIX OBJECT
## The function 'makeCacheMatrix' takes a matrix 'x' as an argument and stores 
## it within a local environment together with its inverse (if calculated). 
## The function returns internal methods to get and retrieve the matrix and its
## inverse. 

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(data){
    x <<- data
    inverse <<- NULL 
  }
  get <- function(){
    return(x)
  }
  
  set_inverse <- function(inv){
    inverse <<- inv
  }
  get_inverse <- function(){
    return(inverse)
  }
  interface <- list(set=set,
                    get=get,
                    set_inverse=set_inverse,
                    get_inverse=get_inverse)
  return(interface)

}


## RETURN INVERSE OF CACHED MATRIX
## The 'cacheSolve' function takes as arguments a 'makeCacheMatrix' object and 
## additional optional arguments for the base 'solve()' function. If the 
## inverse of the cached matrix is not in the object's cache it will be 
## newly calculated; otherwise the stored value will be returned.
cacheSolve <- function(x, ...) {
  inv <- x$get_inverse()
  if(is.null(inv)){
    cat("No cached inverse. Calculating... ")
    data <- x$get()
    inv <- solve(data,...)
    x$set_inverse(inv)
    cat("Done.\n")
    return(inv)
  }
  else{
    cat("Retrieving cached inverse.\n")
    return(inv)
  }
}
