## Put comments here that give an overall description of what your
## functions do

## These functions take a given matrix, A, as input and store the matrix 
## along with its inverse. If the inverse has been computed before then 
## these functions allow us to just return the already computed inverse.

## the function makeCacheMatrix takes as input matrix A, then creates a list
## of four functions that 1) Set the Matrix, 2) Get the matrix, 3) Sets the
## inverse, 4) Gets the inverse

makeCacheMatrix <- function(A = matrix()) {
  inv_A <- NULL
  set <- function(B) {
    A <<- B
    inv_A <<- NULL
  }
  get <- function() A
  setinv <- function(inv) inv_A <<- inv
  getinv <- function() inv_A
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)  
}


## cacheSolve takes as input matrix A, computed from makeCacheMatrix. It then 
## returns the inverse if its already been computed if not it then computes the
## inverse using the solve() function.

cacheSolve <- function(A, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_A <- A$getinv()
  if(!is.null(inv_A)) {
    message("getting cached data")
    return(inv_A)
  }
  data <- A$get()
  inv_A <- solve(data)
  A$setinv(inv_A)
  inv_A  
}