## Since the inverse of a matrix quite costly to compute, these function help by
## caching the inverse, enabling it to be used again and again without having to
## be calculated again.

## Creates a special kind of "matrix" which can cache the inverse of itself.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}


## Computes the inverse of the special kind of "matrix" returned by the function
## makeCacheMatrix. If the calculation has already been made and the underlying
## matrix has not changed since then, the result is returned from the cache.
## Otherwise it's calculated on the spot and cached for future use.

cacheSolve <- function(x) {
  i <- x$getinverse()
  
  if(!is.null(i)) {
    message("retrieving cached inverse")
    return(i)
  }
  
  matrix <- x$get()
  i <- solve(matrix)
  x$setinverse(i)
  i
}
