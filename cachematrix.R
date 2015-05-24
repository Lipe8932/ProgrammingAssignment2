## Calculate the inverse of a matrix and cache it

## Create a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Calculate the inverse of a matrix. 
## check to see if the inverse already has been calculated. 
## If so, get the mean from the cache and skip the computation. 
## Otherwise, calculate the mean of the data and set the value of the inverse
## in the cache via the makeCacheMatrix function.

cacheSolve <- function(x, ...) { ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
