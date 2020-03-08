## Put comments here that give an overall description of what your
## functions do

## my overall function do is set matrix and get it matrix^-1 where it is mean inverse the matrix


## Write a short comment describing this function

#makeCachmatrix function create list of function that is 
#set the matrix value 
#get the matrix value 
#set matrix inverse 
#get matrix inverse 

makeCacheMatrix <- function(x = matrix()) {
  inverse_m <- NULL
  set <- function(y) {
    x <<- y
    inverse_m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse_set) inverse_m <<- inverse_set
  getInverse <- function() inverse_m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## Write a short comment describing this function
##cacheSolve is function return the inverse of matrix 
##the steps is 
#check the inverse is has been computed and if that already inverse then skip
#computation and get the matrix 
#if not setInverse function and computes 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse_m <- x$getInverse()
  if (!is.null(inverse_m)) {
    message("getting cached data")
    return(inverse_m)
  }
  mat <- x$get()
  inverse_m <- solve(mat, ...)
  x$setInverse(inverse_m)
  inverse_m
}
