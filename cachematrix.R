## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  #matrix m
  m <- NULL
  
  #set function for setting a matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  #get function for getting a matrix
  get <- function() x
  
  #setting an inverse matrix to m
  setinverse <- function(inverse) m <<- inverse
  
  #getting an inverse matrix to m
  getinverse <- function() m
  
  #returning a list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  

}

#Caching Inverse Matrix using this function
cacheSolve <- function(x, ...) {
  #using getinverse function to get the matrix from m in cacheMatrix 'x' object and putting it in local matrix 'm'
  m <- x$getinverse()
  
  #checking whether m exists or not , if it does then no need to computer and just returned the cached value
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  #Getting the original matrix
  data <- x$get()
  
  #Computing inverse of original matrix and throwing it in local 'm'
  m <- solve(data, ...)
  
  #Setting inversed matrix onto CacheMatrix object's m variable
  x$setinverse(m)
  
  #returning local matrix that is the inverse of 'x'
  m
}
