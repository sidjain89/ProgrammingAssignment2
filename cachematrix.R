## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setInv <- function(solve) inv <<- solve 
  getInv <- function() inv
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x = matrix(), ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$setInv(inv)
  inv
}


############# test the code ######################

x <- rbind(c(1, 2), c(3, 4))
inv <- makeCacheMatrix(x)
inv$get()

# no cache in first run but will get the "cached" message and data in second run
cacheSolve(inv)

############## end of script ###################