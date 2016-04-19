## Assignment Week3 --> Caching the Inverse of a Matrix

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  inverseMatrix <- NULL
  
  ## set function
  set <- function(m){ 
      x <<- m
      inverseMatrix <<- NULL
  }
  
  ## get function
  get <- function(){
    x
  }
  
  ## setInverse function
  setInverse <- function(inverse){
    inverseMatrix <<- inverse
  }
  
  ## getInverse function
  getInverse <- function(){
    inverseMatrix
  }
  
  ## returned object
  retorno <- list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  retorno
}




## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverseMatrix <- x$getInverse()
  retorno <- NULL
  
  if(!is.null(inverseMatrix)){
    retorno <- inverseMatrix
  } else {
    retorno <- solve(x$get())
    x$setInverse(retorno)
  }
  
  retorno
}
