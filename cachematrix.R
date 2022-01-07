#using the example provided in the assignment I based my code for caching the 
#matrix off of #their mean code. changing the numerical argument in the makecachematrix 
#function establishes a #matrix rather than a vector

#the example provided an excellent template from which to use. 

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y 
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(matrix) inverse <<- matrix
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}        


cacheSolve <- function(x, ...) {
  
  inverse <- x$getinverse()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  #using the solve function as established in the assignment rather than the mean function allows
  #the inverse of the matrix to be taken rather than the mean of the established vector
  matrixdata <- x$get()
  inverse <- solve(matrixdata)
  x$setinverse(inverse)
  inverse  
}