##Matrix inversion is usually a costly computation and their may be 
##some benefit to caching the inverse of a matrix rather than 
##compute it repeatedly.
##makeCacheMatrix is a function than creates a special matrix that can catches 
## its inverse.

##The first function creates a special matrix which is a list containing
##a function to set the value of the matrix, get the value of the matrix
## set the value of the inverse and finally get the value of the inverse.
makeCacheMatrix <- function(x = matrix()) {
  i  <- NULL
  set  <- function(y){
    x <<- y
    i <<- NULL 
  }
  get  <- function() x
  setinverse  <- function(inverse) i  <<- inverse
  getinverse  <- function() i
  list(set= set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
  
}

##cacheSolve is a function that computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. 
cacheSolve <- function(x, ...) {
  i  <- x$getinverse()
  if (!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data  <- x$get()
  i  <- solve(data, ...)
  x$setinverse(i)
  i

## For return a matrix that is the inverse of "X"
##First it calculates if the inverse has been calculated. 
##If so, it gets the inverse and skip the computation. 
##If dont, it calculates the inverse and it sets the value via setinverse.