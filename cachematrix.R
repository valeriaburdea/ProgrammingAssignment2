## The following two functions are meant to cache the inverse of a matrix
## (provided it is invertible) and use this value in case we need it
## for several occasions without recomputing it if it hasn't changed.

## This function creates a list of 4 functions that can provide different 
## information regarding the matrix we feed it. The 4 functions are: 
## set and get the value of the matrix, set and get the inverse of the matrix.

makeCacheMatrix<- function(X=matrix()){
      i<-NULL
      set<-function(Y){
           X<<-Y
           i<<-NULL
      }
      get<-function() X
      setinverse<-function(solve) i<<-solve
      getinverse<-function() i
      list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function computes the inverse of the above matrix but only in case
## it has not changed from the previous computation. If it did, it recomputes
## it using the above function and afterwards resets the value cached above;
## if it didn't it gets the value of the inverse that was cached with the 
## above function.


cacheSolve<-function(X, ...){
      i<-X$getinverse()
      if(!is.null(i)){
           message("getting cached data")
           return(i)
      }
      data<- X$get()
      i<-solve(data, ...)
      X$setinverse(i)
      i
}
