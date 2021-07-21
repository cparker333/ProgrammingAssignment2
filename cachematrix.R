## These functions can be used to store an inverse of a passed matrix such
## the inverse can be retrieved from cache as long as the matrix has not
## changed and the inverse has been previously stored.

## The makeCasheMatrix stores the passed matrix in cache, solves the matrix,
## and stores the inverse of the matrix in cache as well.  The function then 
## returns the same matrix that was passed to it.

makeCacheMatrix <- function(x = matrix()) {
  cachematrix <<- NULL
  lastmatrix <<- NULL
  lastmatrix <<- x
  msolved <- solve(x)
  cachematrix <<- msolved
  print("New values cached")
  return(x)
}


## The casheSolve function retrieves the last matrix, and it's inverse, stored 
## by makeCacheMatrix().  If the passed matrix is different than the cached
## matrix, then the new matrix is sent to makeCacheMatrix and the inverse of
## the new matrix is returned.  If the inverse is already present in cache, the
## cached inverse if returned.  If neither condition is true, then the function
## returns the inverse of the passed matrix.

cacheSolve <- function(x, ...) {
  checkmatrix <<- lastmatrix
  cachecheck <<- cachematrix
  matrixdiff <- checkmatrix - x
  if (sum(matrixdiff[,]) != 0) {
    print("Matrices differ, caching new values")
    makeCacheMatrix(x)
    return(solve(x))
  } else if (!is.null(cachecheck)) {
    print("Returning cached inverse matrix")
    return(cachecheck)
  } else {
    print("No cached values found, calculating new inverse")
    solve(x)
  }
}
