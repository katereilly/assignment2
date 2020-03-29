## The functions calculate the inverse of a matrix, cacheing the outcomes rather than recalculating

## This function creates the matrix to store an inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <-- NULL
  }
  get <- function()x
  setinverse <- function(inverse)
    getinverse <- function()inv
  List (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The function calculates the inverse and stores it in the matrix created above

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv      
}
