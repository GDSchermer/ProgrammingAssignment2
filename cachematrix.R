## makeCacheMatrix This function creates a special "matrix" 
## object that can cache its inverse

## cacheSolve This function computes the inverse of the 
## special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should 
##retrieve the inverse from the cache.

## makeCacheMatrix creates matrix (assume invertable)

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y;
    inverse <<- NULL;
  }
  
  get <- function() x
  setinv <- function(inv) inverse <<- inv
  getinv <- function() inverse
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## cacheSolve -- takes matrix, computes inverse and returns cache if exists, or caches if not

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinv()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinv(inverse)
  inverse
  
}