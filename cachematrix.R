##

## Creat A Matrix with functions

makeCacheMatrix <- function(x = matrix()) {
  inversematrix <- NULL
  set <- function(y){
      x <<- y
      inversematrix <<- NULL
  }
  get <- function(){x} 
  setinverse <- function(inverse){inversematrix <<- inverse}
  getinverse <- function() inversematrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Check if there are cache data already
## If so, get it
## If not, calculate it and set it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inversematrix <- x$getinverse()
  if(!is.null(inversematrix)){
    message("getting cached data")
    return(inversematrix)
  }
  data <- x$get()
  inversematrix <- solve(data)
  x$setinverse(inversematrix)
  inversematrix
}
