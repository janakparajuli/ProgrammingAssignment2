## First part of assignment:- makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {
inverse <- NULL
set <- function(y){
	x <<- y
	inverse <<- NULL
	}
get <- function() x
setinverse <- function(Inverse) inverse <<- Inverse
getinverse <- function() inverse
list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}
