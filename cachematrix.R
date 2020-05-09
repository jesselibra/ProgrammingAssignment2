## These two functions 1. create the cached matrix and 2. cacluate the inverse or retrieve the cached inverse 
## matrix if it has already been calculated. 

## makeCacheMatrix sets and gets the value of the caching matrix
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse
## 4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) i <<- solve
  getInverse <- function() i 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## cacheSolve checks if the inverse exists in the cache matrix, and if so returns it. If not it calculates it and puts
## it in the cache

cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    if(!is.null(i)){
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data,...)
    x$setInverse(i)
    i

        ## Return a matrix that is the inverse of 'x'
}
## m<- matrix(rnorm(4),2,2)
## c <- makeCacheMatrix(m)
## cacheSolve(c)
