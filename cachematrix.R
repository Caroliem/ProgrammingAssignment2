
## This function will create a list of functions on the matrix(x)

makeCacheMatrix <- function(x = matrix()) {
  mInv <<- NULL
  ## function to set matrix, assumption if we set new matrix we also reset inverse to NULL  
  set <- function(y) {
    x <<- y
    mInv <<- NULL
  }
  ## function to get matrix 
  get <- function() x
  ## function to set inverse matrix
  setinverse <- function(inverse) mInv <<- inverse
  ## function to get inverse matrix
  getinverse <- function() mInv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## CacheSolve is function that returns a matrix that is the inverse of 'x'
## input is the list retrieved from makeCacheMatrix
cacheSolve <- function(x) {
  ## retrieve current cached inverse for x
  inverse <- x$getinverse()
  ## check if getinverse exists (ie is not NULL) and in that case retrieve the cached data
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  } else 
  ## inverse doesn't exists and calculate inverse    
    { 
    message("getting new calculated data")
    ## retrieve matrix    
    data <- x$get()
    ## calculate inverse
    inverse <- solve(data)
    ## cache calculated inverse
    x$setinverse(inverse)
    ## print inverse
    inverse
    }  
}