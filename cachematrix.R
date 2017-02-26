## Aim of the functions: caching the inverse of a matrix
## Why: matrix inversion = (usually) costly computation, better to cache the inverse 
## of a matrix rather than compute it repeatedly

## part 1: function makeCacheMatrix
## part 2: function cacheSolve


## Part 1: function makeCacheMatrix
## Modus operandi: function creates a special "matrix" by:  
##   - setting the value of the matrix
##   - getting the value of the matrix
##   - setting the value of the inverse matrix
##   - getting the value of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) im <<- inverse
  getInverse <- function() im
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Part 2: function cacheSolve
## Modus operandi: function computes the inverse of the output of part 1.
## If the inverse has already been calculated (and the matrix 
## has not changed), then it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  im <- x$getInverse()
  if (!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  mat <- x$get()
  im <- solve(mat, ...)
  x$setInverse(im)
  im
}


## Testing:
## source('cachematrix.R')
## x<-matrix(c(1,2,3,4),2,2)
## im <- makeCacheMatrix(x)
## cacheSolve(im)
