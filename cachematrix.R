makeCacheMatrix <- function(mat = matrix()) {
  inv <- NULL
  set <- function(mtrx) {
    mat <<- mtrx
    inv <<- NULL
  }
  get <- function() mat
  set.inverse <- function(setinv) inv <<- setinv
  get.inverse <- function() inv
  list(set = set, get = get,
       set.inverse = set.inverse,
       get.inverse = get.inverse)
}


cacheSolve <- function(cached.mat, ...) {
  inv <- cached.mat$get.inverse()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
    raw.mat <- cached.mat$get()
    inv <- solve(raw.mat, ...)
    cached.mat$set.inverse(inv)
    inv
}
