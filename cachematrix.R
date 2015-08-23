## The functions below shows the usage of super assignmnet operator "<<-" 
## which helps the calling function to assign values to objects in an environment 
## created by a different funtion.

## makeCacheMatrix: This function creates placeholders to holda matrix and inverse matrix.
## The function creates a list of 4 functions
## set: sets value for matrix object
## get: gets the matrix
## setinverse: assigns inverse of matrix
## getinverse: fetches inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL 
  set <- function(y)
  {
    i<<-NULL
    x<<-y
  }
  
  get <- function() x
  
  setinverse <- function(inv)
  {
    i<<-inv
  }
  
  getinverse <- function() i
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## cachesolve: This function is used to calculate inverse of metrix created using 
## makeCacheMatrix function. If inverse of matrix doesn't exist, inverse is calulated 
## using solve() function and  the placeholder for inverse matrix (i) is assigned 
## using <<- . All the further calls for cachesolve, fetch the pre-calculated value from
## cache (makeCacheMatrix environment) instead of calculating again. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
