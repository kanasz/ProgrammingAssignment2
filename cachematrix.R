#This function creates matrix object that caches its inverse
#
#Args:
# x: the matrix to be processed
#
#Returns:
# Returns the list with functions to
#   set the value of the original matrix
#   get the value of the original matrix
#   set the vakue if inverse of the original matrix
#   get the value of inverse of the original matrix

makeCacheMatrix <- function(x = matrix()) {
  
  #inverseMatrix store chached inverse matrix
  inverseMatrix<-NULL
  
  #Setter for matrix
  #When setting new matrix, previously cached inverseMatrix must be set to NULL
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  
  #Getter for matrix
  get  <- function() x
  
  #Setter for inverse matrix matrix
  setinverse  <- function(inverse) inverseMatrix  <<- inverse
  
  #Getter for inverse matrix matrix
  getinverse  <- function() inverseMatrix
  
  list(set= set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}

#This function returns the inverse of the original matrix.
cacheSolve <- function(x, ...) {  
  inverseMatrix  <- x$getinverse()
  #Check if inverse is computed. If so, cached inverse of the matrix is returned. 
  #If not, inverse of the matrix is computed and cached.
  if (!is.null(inverseMatrix)){
    message("getting cached data")
    return(inverseMatrix)
  }
  data  <- x$get()
  inverseMatrix  <- solve(data, ...)
  x$setinverse(inverseMatrix)
  inverseMatrix
}

## Example usage:
## > x <- matrix(rnorm(25), nrow = 5)          
## > cx <- makeCacheMatrix(x)                  
## > cx$get()                                  
## > cacheSolve(cx)                            
## > cacheSolve(cx)                          