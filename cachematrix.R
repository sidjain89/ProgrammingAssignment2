## Put comments here that give an overall description of what your
## functions do

# 1. This function will create a cached matrix
# 2. get and set are used to encapsulate 
# insert and retrieval matrix and its inverse
# 3. << operator takes the value from the global cache.

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


# 1. The following function returns the inverse of the matrix. 
# 2. Checks if inverse was computed by querying the cache
# 3. If inverse exists in cache return the cached value else
# 4. calculate the inverse and insert the value in the cache via setinverse function.

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