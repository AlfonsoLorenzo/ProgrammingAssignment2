## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##makeCacheMatrix
##Parameters: (x = An invertible matrix)
##Description:  Builds a "special" matrix consisting on a list that contains four functions:
##    set:  assigns the parameter to the inner value of x using the operators <<- that causes a search
##        to made through parent environments for an existing definition of the variable being assigned.
##    get: returns the value of the inner matrix
##    setInverse: Calculates the inverse of the inner matrix using solve(x)
##    getInverse: Returns the inverse of the inner matrix

makeCacheMatrix <- function(x = matrix()) {
  
  InverseX <- NULL
  
  set <- function(y) {
    x <<- y
    InverseX <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(mean) InverseX <<- solve(x)
  
  getInverse <- function() InverseX
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## Write a short comment describing this function

##cacheSolve
##Parameters (x = A"special matrix" built with the funcion makeCacheMatrix)
##Description:  Taking a "special matrix" returns the inverse of its inner matrix. 
##              This inverse is calculated when the function is called for first time
##              and obtained from the cache from this point on


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inverseX <- x$getInverse()
      
      if(!is.null(inverseX)) {
        
        message("Getting Cached Matrix Inverse")
        
        return(inverseX)
      }
      
      data <- x$get()
      
      inverseX <- solve(data)
      
      x$setInverse(inverseX)
      
      inverseX
  
}
