## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function creates a list with four function 
# the first is to set the matrix and sec to get the value of this matrix
# third is to set the value of the solve fun from the cache function
# the fourth is to get the value of solve 

makeCacheMatrix <- function(x = matrix()) {
  
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
  
    get <- function() x
    setinverse<- function(sol) m <<- sol
    getinverse <- function() m
    
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        #this checks whether there is an existing value for solve or not
        #if not it calculates the new value and set it in the x matrix
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
}
