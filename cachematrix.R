#Scoping rules
#Create two different functions to cache the inverse of a matrix rather than computing it repeatedly and other function to retrieve/compute inverse of a matrix respectively

#This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  S <- matrix(,nrow=nrow(x),ncol=ncol(x))
  
  #retain the value of source matrix
  set <- function(y) {
    x <<- y
    S <- matrix(,nrow=nrow(y),ncol=ncol(y))
  }
  
  #get the value of cached matrix
  get <- function() x
  
  #setting the value of inverse
  setinverse <- function(sol) S <<- sol
  
  #getting the cached value of inverse
  getinverse <- function() S
  
  #return a vector of actual matrix and inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#This function computes/retrieves the inverse of the special "matrix" returned by 
#makeCacheMatrix depending on whether the inverse cached or not 
cacheSolve <- function(x, ...) {
  #Getting the inverse value to check whether cached or not
  S <- x$getinverse()
  
  #check whether inverse is cached or not (non null value of S mean the existence of cached value)
  if(!is.null(S)) {
    message("getting cached data")
    return(S)
  }
  
  #create a duplicate matrix to calculate inverse
  data <- x$get()
  
  #Calculate inverse if it is not cached
  S <- solve(data, ...)
  x$setinverse(S)
  S
}
