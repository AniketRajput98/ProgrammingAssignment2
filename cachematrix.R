## Put comments here that give an overall description of what your
## functions do

## This function is used to create a special object that stores a matix and caches its inverse.


makeCacheMatrix <- function(x = matrix()) {
  inv_m<-NULL
  set<-function(y){
    x<<-y
    inv_m<<-NULL
  }
  get<-function() x
  setinv<-function(inv) inv_m<<-inv
  getinv<-function() inv_m
  
  list(set=set,get=get,setinv=setinv,getinv=getinv)

}


## The following function calculates the inverse of the matrix created by the above function.

cacheSolve <- function(x, ...) {
        inv_m<-x$getinv()
        if(!is.null(inv_m)){
          print("getting cached data")
          return(inv_m)
        }
        m<- x$get()
        inv_m<-solve(m)
        x$setinv(inv_m)
        inv_m
}
