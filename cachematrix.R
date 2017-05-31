## Put comments here that give an overall description of what your
## functions do


## This function creates an object (a list) with matrix and its cashed inverse

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  setMatrix <- function(mat){
    x <<- mat
    inverseMatrix <<- NULL
  }
  getMatrix <- function() x
  setInvMatrix <- function(invMat) inverseMatrix <<- invMat
  getInvMatrix <- function() inverseMatrix
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInvMatrix = setInvMatrix, getInvMatrix = getInvMatrix)
}

## Cashing an inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invMat <- x$getInvMatrix()
  if (!is.null(invMat)){
    message("Getting cached inverse matrix")
    return(invMat)
  }
  mat <- x$getMatrix()
  invMat <- solve(mat)
  x$setInvMatrix(invMat)
  invMat
}