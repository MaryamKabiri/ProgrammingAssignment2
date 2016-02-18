## These functions creat and store a matrix and cache's its inverse.

## This function creates a list of four functions of set, get, setInverse, and getInverse.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL # store the matrix in cache
    }
  get <- function() x  # get the matrix
  setInverse<- function(solve) m<<-solve  # set the inverse of matrix
  getInverse<-function() m  # get the inverse of matrix
  list (set=set, get=get,
        setInverse=setInverse,
        getInverse=getInverse) # list of four functions created above
}

## This function calculates the inverse of the matrix created by the makeCacheMatrix. First, it checks if th einverse has been 
## calculated before or not. If it was claculated before it recalls the matrix from cache and if not it solves for the inverse 
## via setInverse function and store it in the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m<-x$getInverse()
  if(!is.null(m)){
    message("getting chaced data")
    return(m)  # if the inverse has been calculated sends this message. 
  }
  data<- x$get() # if th einverse has not been calculated get the matrix from makeCacheMatrix
  m<-solve(data, ...) # calculate the inverse of matrix
  x$setInverse(m) # store the calculated inverse into cache 
  m  # return the calculated result
}
