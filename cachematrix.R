## makeCacheMatirx and cacheSolve work together to cache the inverse
## of a matrix so that once computed, the inverse value does not
## need to be re-calculated. 
## example use
## matrixA <-matrix (data = c(5,10,15,20),nrow =2, ncol = 2)
## cm <-makeCacheMtatrix(matrixA)
## cacheSolve(cm)  will return the inverse of matrixA 
## subsequent evalustions of calcSolve(cm) will retrun a message
## "getting cached data" if the matrix has already been calcluated


## makeCacheMatrix creates a special "matrix" object that can
## cach the the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
     mInv<-NULL  # provides default if cacheSolve has not been used, else the inverse result
     set<-function(y){
          x<<-y
          mInv<<-NULL
     }
     get<-function() x # returns the input matrix
     setInvMatrix<-function(solve) mInv<<- solve
     getInvMatrix<-function() mInv
     list(set=set, get=get, # list to: set,get the value of the matrix and
          setInvMatrix=setInvMatrix, # set,get the value of the inverse
          getInvMatrix=getInvMatrix)
}


## cacheSolve checks to see if inverse already calculated
## if has returns cached data
## if has not been run calcluate the  inverse

cacheSolve <- function(x=matrix(), ...) {
     m<-x$getInvMatrix()
     if(!is.null(m)){
          message("getting cached data")
          return(m)
     }
     matrix<-x$get()
     m<-solve(matrix, ...)
     x$setInvMatrix(m)
     m
}
