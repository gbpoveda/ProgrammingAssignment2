

makeCacheMatrix <- functin(x = matrix()) {
        inv <- NULL
        def <- function(z){
                  x <<- z
                  inv <<- NULL
     }
     take <- function() x
     defInv <- function(inverse) inv <<- inverse
     takeInv <- function() inv
     list(def = def, take = take, defInv = defInv, takeInv = takeInv)
}


cacheSolve <- function(x, ...) {
      inv <- x$takeInv()
      if (!is.null(inv)){
          print("cached data taken")
    return(inv)
  }
  m <- x$take()
  inv <- solve(m, ...)
  x$defInv(inv)
  inv
        
       
}
