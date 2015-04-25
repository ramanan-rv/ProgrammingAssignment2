## This program is to write a pair of functions that cache the inverse
## of a matrix.
## For this assignment, it is assumed that the matrix supplied is 
## always invertible. 


## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(matrix = matrix()) {
  
  ## Code to check the matrix is square
  if(dim(matrix)[1] != dim(matrix)[2]){
    print("Error: Arguement passed is not a square matrix") 
    return()
      }
  
  inverse <- NULL     ## Initilize the inverse matrix
  
  ## setmatrix enables changing the matrix with user supplied "newmatrix"
  setmatrix <- function(newmatrix) {
    ## Code to check the matrix is square
    if(dim(newmatrix)[1] != dim(newmatrix)[2]){ 
      print("Error: Arguement passed is not a square matrix") 
      return()
    }
    matrix<<- newmatrix ## Assign value of newmatrix to matrix in cachematrix
    inverse<<- NULL     ## Since matrix changed, previous inverse, if any is nullified
  }
  
  ## getmatrix fetches the current matrix in memory
  getmatrix<- function() matrix
  
  ## setinverse assigns a calculated value to the inverse 
  setinverse<- function(newinverse) inverse<<- newinverse
  
  ## getinverse returns the value of inverse in memory
  getinverse<- function() inverse
  
  ## makeCachematrix returns special matrix list of above 4 functions
  list(setmatrix = setmatrix,
       getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed), then 
## the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(mtxlist, ...) { ## mtxlist is 4 function list from makeCachematrix
## Return a matrix that is the inverse of 'mtxlist'
  inverse<- mtxlist$getinverse()      ## Assign inverse value, if any from cache
  if(!is.null(inverse)){              ## If not null, inverse value present in cache
    print("Returning value of inverse from cache")
    return(inverse)                 ## Return inverse and exit function
  }
  matrix<- mtxlist$getmatrix()      ## Fetch matrix for inverse calculation
  inverse<- solve(matrix)           ## Calculate inverse
  mtxlist$setinverse(inverse)       ## Cache inverse in mtxlist
  inverse                           ## Return calculated inverse
}
