makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}




cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached result")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}


## ---------------Checking the program-----------------------
m <- matrix(rnorm(16),4,4)
m1 <- makeCacheMatrix(m)
cacheSolve(m1)

           [,1]       [,2]      [,3]       [,4]
[1,] -0.3817589  1.1210649 0.2267649 -0.3054817
[2,] -0.8369747 -0.5172706 1.4302236 -1.6357711
[3,]  0.3069499 -3.1123850 2.0783027 -2.6848745
[4,] -0.2667230 -2.7365810 4.7258180 -3.2100194
