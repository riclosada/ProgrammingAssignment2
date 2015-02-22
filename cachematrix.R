
## Cache matrix inverse for faster computation
## Author: Ricardo Losada, February 2015

## Test with:
##  N <- 500L
##  A <- matrix(rnorm(N*N),N,N)
##  Ac <- makeCacheMatrix(A) 
##  system.time(AI <- cacheSolve(Ac))  # First computation; slower
##  system.time(AI2 <- cacheSolve(Ac)) # Subsequent computation; fast
##  norm(AI-AI2)                       # Test that both answers match
##  norm(A%*%AI-diag(N))               # Test that inverse is close to theoretical


## makeCacheMatrix - Create a list that contains 4 functions:
## a set/get function for a matrix and a setinverse/getinverse that
## provide access to the inverse. Initialize the inverse to NULL
makeCacheMatrix <- function(A = matrix()) {
  AI <- NULL
  set <- function(B) {
    A <<- B
    AI <<- NULL
  }
  get <- function() A
  setinverse <- function(BI) AI <<- BI
  getinverse <- function() AI
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve - Return the inverse of the matrix cached with makeCacheMatrix
## If the cached inverse is NULL then:
## 1. compute the inverse (using slove)
## 2. cache it for future access (using setinverse) 
## 3. return the inverse as a result
cacheSolve <- function(Ac, ...) {
  AI <- Ac$getinverse()
  if(!is.null(AI)) {
    message("getting cached inverse")
    return(AI)
  }
  A <- Ac$get()
  AI <- solve(A, ...)
  Ac$setinverse(AI)
  AI
}
