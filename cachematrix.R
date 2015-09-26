## Assignment: Caching the Inverse of a Matrix
##Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). Your assignment is to write a pair of functions that cache the inverse of a matrix.
##Write the following functions:
  
##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
##Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse.

##For this assignment, assume that the matrix supplied is always invertible.

##makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## 1.set the value of the matrix
## 2.get the value of the matrix
## 4.get the value of inverse of the matrix
## 3.set the value of inverse of the matrix

makeCacheMatrix <- function(x=matrix()){
  inve <- NULL
  set <- function(y){
    x <<- y
    inve <<- NULL
  }
  get <- function()x
  setinve <- function(inverse) inve <<- inverse
  getinve <- function()inve
  list(set=set, get=get, setinve=setinve, getinve=getinve)
}

# The following function returns the inverse of the matrix. First it checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinve function.

cacheSolve <- function(x, ...){
  inve <- x$getinve()
  if(!is.null(inve)) {
    message("getting cached data")
    return(inve)
  }
  data <- x$get()
  inve <- solve(data, ...)
  x$setinve(inve)
  inve
}

##running function

> newmatrix <- makeCacheMatrix(matrix(rnorm(10), 2, 2))
> newmatrix$get() 
[,1]       [,2]
[1,] -0.1454236 -0.1949505
[2,]  0.5858757  0.1371903
> newmatrix$getinve()
NULL
> cacheSolve(mewmatrix)
[,1]      [,2]
[1,]  1.455352  2.068088
[2,] -6.215128 -1.542693


