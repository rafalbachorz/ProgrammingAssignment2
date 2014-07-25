## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
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


## Write a short comment describing this function
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  cachedValue <- x$getInverse()
  if(!is.null(cachedValue)) {
    message("getting cached data")
    return(cachedValue)
  }
  data <- x$get()
  #cachedValue <- mean(data, ...)
  cachedValue <- solve(data, ...)
  x$setInverse(cachedValue)
  cachedValue
}
sampleVector=rnorm(400)
rm(commands, m, xx)

xx<-matrix(sampleVector, nrow=20, ncol=20)
commands<-makeCacheMatrix(xx)
cacheSolve(commands)