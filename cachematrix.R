## makeCacheMatrix returns a list of function to be the params in cacheSolve
## 
##
## cacheSolve get the inverse of the matrix created in makeCacheMatrix() and returns either the rruntime value 
## or from the cache if has already been calculated.



## Write a short comment describing this function
## Param: invertible matrx
## Output: 
##  1. set the matrix
##  2. get the matrix
##  3. set the inverse
##  4. get the inverse
##this list is used as the param to cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) inv <<- inverse
      getinverse <- function() inv
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Param: makeCacheMatrix Funtion output matrix
## Output: inverse of the original matrix input in makeCacheMatrix Funtion

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getinverse()
      if (!is.null(inv)) {
            message("Getting Cached Data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinverse(inv)
      inv
}
