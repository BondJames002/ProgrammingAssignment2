## Put comments here that give an overall description of what your
## functions do

##This function creates a matrix that can cache its inverse. This can sigificantly improve processing speed if there are

makeCacheMatrix <- function(x = matrix()) {  
    m  <- NULL                              
    set <- function(y) {                    
      x <<- y                   ## assign new value of matrix in parent environment          
      m <<- NULL                ## if not new matrix, set m back to NULL          
    }
    get <- function() x                     
    
    setinverse <- function(inverse) m <<- inverse  ## value of m assigned in parent environment
    getinverse <- function() m                     ## gets the value of m

    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)   
    
    
}


##  ## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
   inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}  
