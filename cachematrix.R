## The following functions compute inverse of a matrix and cache the 
## value of the matrix and its inverse. When needed, the inverse is
##available from cache. In case of new matrix, inverse is calculated.


## makeCacheMatrix function does four works and returns a list:
##  set the value of input matrix
##  get the value of input matrix
##  set the value of inverse of matrix
##  get the value of inverse of matrix


makeCacheMatrix <- function(x = matrix()) { 
  invmat <- NULL
  set <- function(y){
    x <<- y
    invmat <<- NULL
  }
  get <- function(){ 
      x
  }
  setinvmat <- function(inv){
    invmat <<- inv
  }
  getinvmat <- function(){
    invmat
  }
  list( set = set, get = get,
       setinvmat = setinvmat,
       getinvmat = getinvmat)
    
}


## The cacheSolve function calculates and returns an inverse of
## matrix. The input it gets from makeCacheMatrix. If the inverse
## of the matrix is available in cache that is returned.

cacheSolve <- function(x, ...) {
   invmat <- x$getinvmat()
  if(!is.null(invmat)) {
    message("getting cached data")
    return(invmat)
  }
  data <- x$get()
  invmat <- solve(data, ...)
  x$setinvmat(invmat)
  invmat
}

