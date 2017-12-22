## Chris Frisbie 
## Assignment 2
getwd()
setwd("~/Documents/GitHub/ProgrammingAssignment2")

## Put comments here that give an overall description of what your
## functions do

##  Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of 
## a matrix rather than computing it repeatedly. my assignment was to write a pair of functions that 
##cache the inverse of a matrix.

## The two functions I wrote are makeCacheMatrix and cacheSolve.
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.


## Write a short comment describing this function

##The first function, makeVector creates a special "vector", which is really a list containing a function to:
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse of the matrix
## 4.  get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set=set, get=get, 
       setinverse=setinverse, 
       getinverse=getinverse)
}


## Write a short comment describing this function
## The following function calculates the inverse of a matrix created with the above function. 
## However, it first checks to see if the inverse of the has already been created and stored in cache. 
## If so, it gets the inverse of the matrix from cache and skips the computation. Otherwise, 
## it calculates the inverse of the matrix and sets the inverted matrix in the cache via the setinverse function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached Matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
