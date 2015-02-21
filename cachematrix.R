## Funtions makeCacheMatrix and cacheSolve are created to 
## cache the inverse of a matrix.

## The first function creates a special "matrix" object which
## can cache the inverse of a matrix. The output of makeCacheMatrix 
## is list containg functions to set the matrix, get the matrix,
## set the inverse and get the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y=matrix()) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)   
}

## The second function calculates and returns the inverse of a special "matrix"
## created with first function. Firstly we check if the inverse has
## been calculated. If so, it gets it from the cache and does not
## repeat calculation. If not, calculate the inverse and set the value 
## via setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
