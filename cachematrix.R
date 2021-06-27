## Two functions that cache the inverse of a matrix

## Write a short comment describing this function

makeCacheMatrix <- function(m = matrix()) {
  i <- NULL
  #Setting the function
  set <- function(matrix){
    m <<- matrix
    i <<- NULL
  }
  #Getting the function
  get <- function() {
    m
  }
  #Setting the inverse
  setInverse <- function(inverse){
    i <<- inverse
  }
  #Getting the inverse
  getInverse <- function(){
    i
  }
  #List of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...)) {
  m <- x$getInverse()
  # Inverse of matrix of 'x
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  ## Get the matrix from our object
  data <- x$get()
  # Calculate the inverse of matrix
  m <- solve(data) %*% data
  ## Set the inverse to the object
  x$setInverse(m)
  #Returning the matrix
  m
        
}
