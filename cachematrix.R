## Pair of functions that cache the inverse of a matrix

## Function to create matrix object to cache its inverse

makeCacheMatrix <- function(m = matrix()) {
  inv<-NULL
  set<-function(mat) {
    m<<-mat
    inv<<-NULL
  }
  get<-function() {
    m
  }
  setinverse<-function(inverse) {
    inv<<-inverse
  }
  getinverse<-function() {
    inv
  }
  list(set=set,get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}



## Function to compute inverse of specialmatrix retuned by makeCacheMatrix function

cacheSolve <- function(m, ...) {
  inv<-m$getinverse()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  data<-m$get()
  inv<-solve(data,...)
  m$setinverse(inv)
  inv
}      

