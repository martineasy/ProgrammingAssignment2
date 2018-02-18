## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  
  ## Return original matrix
  get <- function() x
  
  ## Set inverse
  setinverse <- function(inverse) i <<- inverse
  
  ## Get inverse (may return NULL)
  getinverse <- function() i
  
  ## Returns list object with matrix function
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  ##Check for previously cached inverse
  if(!is.null(i)){
    message("getting cached solve")
    return(i)
  }
  ##
  data <- x$get()
  i <- solve(data,...)
  x$setinvers(i)
  i
}
