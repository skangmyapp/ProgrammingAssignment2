################################################################################################################
## Matrix inversion with cache
################################################################################################################
## Perform matrix inversion like solve
## Since the computation is costly, we avoid redundant computation by creating cache
##
## Functions:
## makeCacheMatrix = matrix-like 'data type' with caching mechanism
## cacheSolve = solve function which will always try to return cached solution before actually running solve
##
## Assumptions: 
## - cacheSolve will only perform matrix inversion
## - arg x is a inversible matrix 
################################################################################################################


## special matrix for caching which can contain cached inversed matrix (if performed before)
makeCacheMatrix <- function(x = matrix()){
  ## placeholder for caching
  inv <- NULL
  
  ## to store new matrix
  set <- function(y){
    x <<- y
    ## reset cached solution 
    ## since matrix has been changed
    inv <<- NULL
  }

  ## to retrive matrix  
  get <- function() x
  
  ## to cache solution for current matrix
  setsolve <- function(solve) inv <<- solve
  
  ## retrieve cached solution
  getsolve <- function() inv
  
  ## return a list with set of functions above
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## cacheSolve like solve but will always try to return cached solution before actually running solve
## argument x should be makeCacheMatrix
cacheSolve <- function(x, ...){
  ## Return a matrix that is the inverse of 'x'
  
  ## check for cached data
  ## return cached data if exists
  inv <- x$getsolve()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## no cached data, perform solve 
  data <- x$get()
  inv <- solve(data, ...)
  ## then cache the solution before return
  x$setsolve(inv)
  inv
}

