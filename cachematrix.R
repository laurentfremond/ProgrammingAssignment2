#make a function that for any entry 'x' will create a list of functions to be used later
makeCacheMatrix <- function(x = numeric()) {
  inv <- NULL # inv is set to be null
  set <- function(dat,a,b) x <<- matrix(dat,a,b) # this function creates a matrix 'x'
  get <- function() x  # this function is used for printing the content of the matrix               
  setinv <- function() inv <<- solve(x) # this function computes the inverse of x
  getinv <- function() print(inv) # this function is used for printing the inverse
  # function names
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


# this function is used to retrieve the inverse from the cache. 
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv) # retrieve the inverse from the cache
  }
  data <- x$get() # retrieve the matrix 'x'
  inv <- solve(data) # return a matrix that is the inverse of data
  inv 
}
