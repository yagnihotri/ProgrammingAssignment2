# MakeCacheMatrix creates a special object which is actually a list. 
# This special object saves environment(parent) where "inverse" object is saved 
# When cacheSolve is called it checks for whether the "inverse" object already exist
# in parent ( globalenviron). If the "inverse" object exist in global envrironment, it is returned otherwise
# inverse is calculated and "set" in the global environment to be retreived later as needed.


## Create a special object that is list of functions to get data, set inverse and get inverse functions


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  get <- function() x
  setinv <- function(inver) i <<- inver
  getinv <- function() i
  list( get = get,
       setinv = setinv,
       getinv = getinv)

  
}


## function to check if inverse matrix already exists in cache, otherwise calculate and return it. 
## Argument to cacheSolve is list from makeCacheMatrix

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
        ## Return a matrix that is the inverse of 'x'
}
