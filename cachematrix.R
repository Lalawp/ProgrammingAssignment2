##makeCacheMatrix provides the framework for placing the matrix and 
##inverse of the matrix in the cache
makeCacheMatrix <- function(m = matrix()) {
  #intializes inverse function
  i <- NULL
  
  #sets matrix values in parent environment
  set <- function(matrix) {
    m <<-matrix
    i <<- NULL
  } 
  
  #gets value of matrix
  get <- function(){
    m
  }
  
  #sets inverse values in parent environment after calculated
  setInverse <- function(inverse){
    i <<- inverse
  }
  #gets values of inverse
  getInverse <- function(){
    i
  }
  #
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}
## cacheSolve provides the inverse of a matrix if the value is not 
## already in the cache; if the value is in the cache, 
## it simply provides the cached answer
cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  
  ##if the value of m is not null, then the inverse matrix has already been calculated
  ##so it just returns the cached result
  if(!is.null(m)){
    print("Returning Cached")
    return(m)
  }
  ##if m is null, cacheSolve will solve for the inverse of the matrix x
  else {
    print("Calculating Inverse")
    data <- x$get()
    m <- solve(data)
    x$setInverse(m)
    m
  } 
}
