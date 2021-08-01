## The aim of this assignment is to write a pair of functions that catch the inverse of the matrix# 


makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function() j 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

  ## Same here, changed "mean" to "solve" and "m" to "s"

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  j <- x$getInverse()
  if(!is.null(j)){
    message("getting cached data")
    return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j
}


## The function makecachematrix create a special "matrix" object in R that can
## catches the invesre of invertible matrix. 

 cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
   inv <- x$getInverse()
   if (!is.null(inv)) {
     message("getting cached data")
     return(inv)
   }
   mat <- x$get()
   inv <- solve(mat, ...)
   x$setInverse(inv)
   inv
 }
 
 
 ## Checking the funtions 

 my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
 my_matrix$get()
 
 
 my_matrix$getInverse()
 
 cacheSolve(my_matrix)
 
 
 cacheSolve(my_matrix)
 
 my_matrix$getInverse()
 
 
 my_matrix$set(matrix(c(2, 2, 1, 4), 2, 2))
 my_matrix$get()
 
 my_matrix$getInverse()
 cacheSolve(my_matrix)
 cacheSolve(my_matrix)
 
 my_matrix$getInverse()
 
 
 
 
 
 
 
 