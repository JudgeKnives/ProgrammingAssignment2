## These functions make a special matrix that can cache its inverse. makeCacheMatrix
## generates the special matrix, and cacheSolve will find the inverse of the matrix and 
## cache it. If the inverse is already cached, cacheSolve will retrieve the inverse from cache.

## makeCacheMatrix function returns a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) 
{
  #initialize inverse to NULL 
  inv <- NULL
  
  #set function sets the matrix in the function environment
  set <- function(x)
  {
    x <<- x
    inv <<- NULL
  }
  
  #get function returns the original matrix passed into makeCacheMatrix function
  get <- function() x
  
  #setinverse function sets the 'inv' variable to the value of 'inverse'
  setinverse <- function(inverse) inv <<- inverse 
  
  #getinverse returns the value of inv in the makeCacheMatrix environment
  getinverse <- function() inv
  
  #returns a list of of the functions in the MakeCacheMatrix environment
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix. If the inverse has already been calculated 
## (and the matrix has not changed), then the cacheSolve should retrieve 
## the inverse from the cache.
cacheSolve <- function(x, ...) 
{
  #initializes inverse in cacheSolve environment to value of inverse stored in x
  inv <- x$getinverse()
  
  #checks to see if the inverse has been cached already
  #if inverse is cached, return the cached inverted matrix
  if(!is.null(inv))
  {
    ##This lets you know that the inverse was previously cached and returns the inverse
    message("getting cached data")
    return(inv)
  }
  ## if inverse is not cached, calculates the inverse and caches it
  else
  {
    #initializes 'data' to be the original matrix passed into makeCacheMatrix
    data <- x$get()
    
    #sets 'inv' to be the inverse of the original matrix
    #solve(x) calculates the inverse of a matrix x 
    inv <- solve(data,...)
    
    #caches the inverse
    x$setinverse(inv)
    
    ## Return a matrix that is the inverse of 'x'
    inv
  }
       
}
