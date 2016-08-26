## The functions makeCacheMatrix and cacheSolve work together to create a square invertible matrix.
## Then make the inverse of the matrix available in cache environment.

## makeCacheMatrix function creates a matrix that can cache it's inverse
## and returns a matrix with list of functions to get/set the inverted matrix in cache

makeCahceMatrix <- function(x = matrix()){
 
  ##stores the cached value
  ##initialize to NULL
  cache <- NULL 
  
  ##create matrix in working environment
  set <- function(y){
    x <<- y
    cache <<- NULL
  }
  
  ##get the value of matrix
  get <- function() x
  
  ##invert the matrix & store in cache
  setInverse <- function(solve) cache <<- solve
 
   ##get the inverted matrix 
  getInverse <- function() cache

##return the created functions to the working environment
  
list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve computes the inverse of the matrix created in makeCacheMatrix
## it returns the cached inverse, if the inverse is already calculated
cacheSolve <- function(x=matrix(),...){
  
  ## get the inverse of the matrix stored in cache
  cache <- x$getInverse()
  
  ##return cached matrix if it is already computed
  if(!is.null(cache)){
    message("Inverse matrix is cached")
    return(cache)
  }
  
  ##compute inverse of matrix
  inv_matrix <- x$get()
  cache <- solve(inv_matrix,...)
  
  ##cache inverse
  x$setInverse(cache)
  
  ##return inverse of matrix
  cache
}
## Sample output

##> cache <- matrix(c(0, 2, 1, 0), nrow = 2, ncol = 2, byrow = TRUE)

##cache
##    [,1] [,2]
##[1,]    0    2
##[2,]    1    0
##> m <-makeCahceMatrix(cache)
##> cacheSolve(m)
##     [,1] [,2]
##[1,]  0.0    1
##[2,]  0.5    0
