## This function implements a special matrix, which caches its inverse,
## to avoid the expensive recalulation
## When the matrix is changed, the inverse is recalculated

makeCacheMatrix <- function(x = matrix()) {
  cached_inverse <- NULL
  
  ## Set new matrix value
  ## Reset inverse to Null to indicate recalculation
  set <- function(y)
  {
    x <<-y
    cached_inverse <<- NULL
  }

  get <- function() x
  
  ## Return cached inverse
  getinverse <- function() cached_inverse
  ## Set cached inverse
  setinverse <- function(inverse) cached_inverse <<-inverse
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Computes the inverse of matrix x
## If x contains a cached inverse, 
## no calculation is performed and the cached value returned
## Assumption: x is square invertible

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        
        ## Cached inverse available
        if (!is.null(i))  {
          return (i)
        }
        
        ## No cached inverse available, perform inversion
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
}
