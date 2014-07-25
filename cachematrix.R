## The functions below can be used for the calculation of the inverse of the invertible matrix
## Due to the fact that the calculation of the invertion is quite costly 
## (scales like n^3, where n is a size of dimension of the matrix) it is a good idea to compute it 
## once and then access whenever needed.

## makeCache matrix function creates a special object (cachedValue) that can be accessed 
## (read or modified) from a different environment.
makeCacheMatrix <- function(x = matrix()) {
  cachedValue <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) cachedValue <<- inverse
  getInverse <- function() cachedValue
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)  
}


## cacheSolve function actually computes the inverse, but only if there is no chached solution.
## In case when the cachedValue is already computed, it returns that value. In the other case 
## (the cachedValue is empty; null) it attempts to get the inverse by using the solve function of R.
cacheSolve <- function(x, ...) {
  cachedValue <- x$getInverse()
  if(!is.null(cachedValue)) {
    message("getting cached data")
    ## Return a matrix that is the inverse of 'x' (cached value)
    return(cachedValue)
  }
  data <- x$get()
  cachedValue <- solve(data, ...)
  x$setInverse(cachedValue)
  ## Return a matrix that is the inverse of 'x' (freshly computed value)
  message("computing data")
  cachedValue
}
sampleVector=rnorm(144)
rm(commands, m, xx, cachedValue)

xx<-matrix(sampleVector, nrow=20, ncol=20)
commands<-makeCacheMatrix(xx)
cacheSolve(commands)
