# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverseerse of the matrix
# 4. get the value of inverseerse of the matrix

makeCacheMatriX <- function(X = matriX()) {
  inv <- NULL
  set <- function(Y) {
    X <<- Y
    inv <<- NULL
  }
  get <- function() X
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

#We assue that matrices are always invertible.

cacheSolve <- function(X, ...) {
  inv <- X$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- X$get()
  inv <- solve(data)
  X$setinverse(inv)
  inv    
}


