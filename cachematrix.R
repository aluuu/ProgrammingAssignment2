## cachematrix.R - functions to create special matrix objects and calculating its' inverse matrix

## makeCacheMatrix - creates a special matrix which can cache its' inverse
## 
## Arguments 
##   x - object of matrix class
##
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  
  set <- function(y) {
    x <<- y
    s <<- NULL
  }  
  
  get <- function() x
  setSolve <- function(newSolve) s <<- newSolve
  getSolve <- function() s
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}

## cacheSolve - calculates inverse matrix of given matrix. If inverse matrix is calculating 
##   for the first time, inverse matrix would be cached. If it was calculated earlier,
##   cached inverse matrix would be returned.
##
## Arguments
##   x - matrix, which was created with makeCacheMatrix function
##
cacheSolve <- function(x, ...) {
  if(is.null(x$getSolve())){
    x$setSolve(solve(x$get(), ...))
  }
  x$getSolve()
}