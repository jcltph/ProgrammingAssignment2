## makeCacheMatrix creates a special "matrix" object associated with the input
## matrix x so that it can cache its inverse. cacheSolve takes the object 
## returned by makeCacheMatrix function and checks the cached value. 
## If the inverse already exists in the cache, the stored value is returned
## rather than recomputed. If not, the new inverse value is computed, 
## stored and returned. 

## makeCacheMatrix creates a special "matrix" object, which is really a list 
## containing a function to set and get the value of the matrix and 
## set and get the value of the inverse.
makeCacheMatrix <- function(x = matrix()) {
     # Creates a special "matrix" object that can cache its inverse.
     #
     # Usage: 
     #    makeCacheMatrix(x = matrix)
     # Args:
     #    x: squre numeric matrix 
     # Returns:
     #    A list containing four functions: 
     #         set(y): set the value of the matrix to y in the cache
     #         get(): get the value of the matrix
     #         setInverse(inverse): set the value of the inverse matrix
     #                              to inverse in the cache
     #         getInverse(): get the value of the inverse matrix
     
     # Define set, get, setInverse, getInverse functions.
     inv <- NULL
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     get <- function() x
     setInverse <- function(inverse) inv <<- inverse
     getInverse <- function() inv
     
     # Output the list of above four functions defined.
     list(set = set, get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}


## cacheSolve function computes the inverse of the special "matrix" object  
## returned by makeCacheMatrix. It first checks to see if the inverse has 
## already been calculated. If so, it gets the inverse from the cache and  
## skips the computation. If not, it calculates the inverse of the data and  
## sets the value of the inverse in the cache via the setInverse function.
cacheSolve <- function(x, ...) {
     # Computes the inverse of the special "matrix" object returned 
     # by makeCacheMatrix.
     #
     # Usage:
     #         cacheSolve(x, ...)
     # Args:
     #         x: special "matrix" object returned by makeCacheMatrix.
     # Returns:
     #         Inverse matrix of an original matrix associated with 
     #         the special matrix object x.
     
     # If the inverse already exists in the cache, return the exiting value.
     inv <- x$getInverse()
     if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
     }
     
     # If no stored value, the inverse is computed, stored and returned.
     data <- x$get()
     inv <- solve(data, ...)
     x$setInverse(inv)
     inv
}


