## This script has two functions similar to the "makemeanVector" example of 
## the course.

## makeCaheMatrix creates the object that keep information of 
##  QueryMatrix (x) : is the matrix that we want to inverse
##  InverseMatrix (m) : is the inverse of the matrix saved in the cache
##  OriginalMatrix (original) : is original matrix we from which we built 
##                              the InvereMAtrix(m).used  to compare 
##                              (when calling the function the nex time)
##                              if the matrix we want to inverse has not 
##                              changed
## the nested functions are getters and setters of the attributes

## cacheSolve a cacheMatrix object (created by mackeCahceMAtrix function)
## and if we have already calculated the inverse of the queryMatrix
## (and this matrix has not changed -identical to originalMatrix- )
## the inverse cached matrix is returned 
## otherwise we calculate the inverse of the queryMatrix and save
## the result and the query matrix as the original matrix 



## makeCaheMatrix creates the object that keep information of 
##  QueryMatrix (x) : is the matrix that we want to inverse
##  InverseMatrix (m) : is the inverse of the matrix saved in the cache
##  OriginalMatrix (original) : is original matrix we from which we built 
##                              the InvereMAtrix(m).used  to compare 
##                              (when calling the function the nex time)
##                              if the matrix we want to inverse has not 
##                              changed
## the nested functions are getters and setters of the attributes
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  original <- NULL
  setQueryMatrix <- function(y) {
    x <<- y
    m <<- NULL
    original <<- NULL
  }
  getQueryMatrix <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  setOriginalMatrix <- function(om) original <<- om
  getOriginalMatrix <- function() original
  list(
       setOriginalMatrix = setOriginalMatrix, 
       getOriginalMatrix = getOriginalMatrix,
       setQueryMatrix = setQueryMatrix, 
       getQueryMatrix = getQueryMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}



## cacheSolve a cacheMatrix object (created by mackeCahceMAtrix function)
## and if we have already calculated the inverse of the queryMatrix
## (and this matrix has not changed -identical to originalMatrix- )
## the inverse cached matrix is returned 
## otherwise we calculate the inverse of the queryMatrix and save
## the result and the query matrix as the original matrix 

cacheSolve <- function(x, ...) {
  inverseMatrix <- x$getInverse()
  ##if the inverse of  matrix queried  is cached we must be sure that 
  ## the matrix queried has not changed (must be identical to the original matrix)
  if(!is.null(inverseMatrix) && identical(x$getQueryMatrix(),x$getOriginalMatrix())) {
    return(inverseMatrix)
  }
  queryMatrix <- x$getQueryMatrix()
  inverseMatrix <- solve(queryMatrix)
  x$setInverse(inverseMatrix)
  x$setOriginalMatrix(queryMatrix)
  inverseMatrix
}