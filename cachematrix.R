## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix defines the functions 1)set, 2)get, 3)compute_inverse and
## 4)get_inverse that will be used in the function cacheSolve.
## 1)set finds the value of the matrix x in another environment
## 2)get gets the value of the matrix from the arguments of the function
## 3)compute_inverse computes the inverse of the matrix using solve(x)
## 4)get_inverse retrieves the inverse of the matrix in some environment
## The function returns a list of functions cited above

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {
    x
  }
  compute_inverse <- function(solve){ 
    inv <<- solve
  }
  get_inverse <- function() {
    inv
  }
  list(set=set, get=get, compute_inverse = compute_inverse, get_inverse = get_inverse)
}


## cacheSolve function computes the inverse of the matrix
## returned from the function makeCacheMatrix.
## It first checks if the inverse already exists in some environment using
## the get_inverse function defined previously. If the inverse already
## exists it returns it. If the inverse of the matrix does not exist
## in any environment it computes it using the compute_inverse function.
## The function returns the inverse of the indicated matrix 

cacheSolve <- function(x, ...) {
        
  inv <- x$get_inverse()
  if(!is.null(inv)) {
    return(inv)
  }
  x_to_use <- x$get()
  inv <- solve(x_to_use,...) 
  x$compute_inverse(inv)
  inv
}
