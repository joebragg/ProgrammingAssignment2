## The below functions cache a matrix and calculates the inverse of that function
## which is also stored in the cache.
##
## The makeCacheMatrix() is an objective function which initializes the cached
## matrix by either passing the matrix via the function argument or by
## calling the setmatrix() function within makeCacheMatrix().
##
## The getMatrix() function returns the original matrix.
##
## The setinvmatrix() function stores the inverse matrix via the cacheSolve() function.
##
## The getinvmatrix() function returns the inverse matrix of the original matrix.
##
## Example command line input below:
##
## > x<-matrix(c(1,2,2,1),nrow=2,ncol=2) 
## > a<-makeCacheMatrix(x)
## > a$setmatrix(x)
## > a$getmatrix()
## [,1] [,2]
## [1,]    1    2
## [2,]    2    1
## > a$getinvmatrix()
## NULL
## > cacheSolve(a)
## > a$getinvmatrix()
## [,1]       [,2]
## [1,] -0.3333333  0.6666667
## [2,]  0.6666667 -0.3333333
## > cacheSolve(a)
## getting cached inverse matrix
## [,1]       [,2]
## [1,] -0.3333333  0.6666667
## [2,]  0.6666667 -0.3333333
## > 

makeCacheMatrix <- function(matrix = matrix()) {
  
  invmatrix <- NULL
  
  setmatrix <- function(y){
    matrix <<- y
    invmatrix <<- NULL
  }
  
  getmatrix <-function() matrix
  
  setinvmatrix <- function(inv) invmatrix <<- inv
  
  getinvmatrix <- function() invmatrix
  
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinvmatrix = setinvmatrix,
       getinvmatrix = getinvmatrix)
}

## The cacheSolve() function checks to determine if an inverse matrix has
## already been stored. If so, it returns the cached inverse matrix. If
## not, it calculates the inverse matrix using the solve() function and
## stores it in the cache using the setinvmatrix() function in
## makeCacheMatrix() function above.

cacheSolve <- function(matrix, ...) {
  ## Return a matrix that is the inverse of 'x'
  invmatrix <- matrix$getinvmatrix()
  
  if(!is.null(invmatrix)){
    message("getting cached inverse matrix")
    return(invmatrix)
  }
  
  data <- matrix$getmatrix()
  invmatrix <- solve(data)
  matrix$setinvmatrix(invmatrix)
}
