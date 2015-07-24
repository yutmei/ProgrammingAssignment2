## Put comments here that give an overall description of what your
## functions do
## This is a pair of functions that cache the inverse 
## of a matrix.

## Write a short comment describing this function
## The makeCacheMatrix function creates a special "matrix" object 
## that can cache its inverse. It creats custom matrix and
## stores matrix in cache. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
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


## Write a short comment describing this function
## The cacheSolve function computes the inverse of the 
## special "matrix" returned by makeCacheMatrix above. It
## will search for the inverse from the cache and retrieve
## it if the inverse has been calculated (and the matrix 
## has not changed), otherwise, the function will 
## calculate the inverse of matrix and store it in cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
        ## Return a matrix that is the inverse of 'x'
}

