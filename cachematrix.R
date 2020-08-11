#This function creates an object (matrix) that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
     
     inverse <- NULL
     set <- function(y){
          x <<- y
          inverse <<- NULL
     }
     get <- function() x
     setInverse <- function(solveMatrix) inverse <<- solveMatrix
     getInverse <- function() inverse
     list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}

#This second step computes the inverse of the object created in step one.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     inverse <- x$getInverse()
     if(!is.null(inverse)){
          message("getting cached data")
          return(inverse)
     }
     data <- x$get()
     inverse <- solve(data)
     x$setInverse(inverse)
     inverse   
}
