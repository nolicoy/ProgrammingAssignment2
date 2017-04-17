## The assignment is to provide an R function that will
## cache potentially long-running or time-consuming
## computations. However, if a value has already been 
## calculated, the function will return that value, 
## avoiding the costly computation process but if not, 
## it will continue and cache the computed value, so it
## can be return quickly at a later time. 
 
## The first function, makeCacheMatrix, creates a vector
## that will hold the inverse of a matrix object. It 
## goes through setting up and getting the matrix, 
## setting and setting the inverse and then is used an
## input to the next function, cacheSolve().

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
  x<<-y
  m<<-NULL
}
get<-function() x
setmatrix<-function(solve) m<<- solve
getmatrix<-function() m
list(set=set, get=get,
   setmatrix=setmatrix,
   getmatrix=getmatrix)
}

## The second function, cacheSolve, receives
## the input of values from makeCacheMatrix.
## It checks if the input is already cached.
## If it is in memory, it will return the inverse
## otherwise, it continues with the computation
## and returns the result.
 
cacheSolve <- function(x=matrix(), ...) {
    m<-x$getmatrix()
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setmatrix(m)
    m
}
