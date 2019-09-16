## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


#take the matrix as an input
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL                             ## initialize inv as NULL; will hold value of matrix inverse 
  set <- function(y) {                   
    x <<- y                            
    inv <<- NULL                       
  }
  get <- function() x                    ## returns value of the matrix argument
  
  Setinverse <- function(inverse) inv <<- inverse  ## assigns value of inv
  Getinverse <- function() inv                     ## gets the value of inv where called
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ## you need this in order to refer to the functions with the $ operator
}


## Write a short comment describing this function
##If the inverse has already been calculated then cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$Getinverse()
  if(!is.null(inv)) {
    message("Cached Data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
