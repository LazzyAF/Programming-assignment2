## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
          inv <- NULL   
          set <- function(y){
            x <<- y
            inv <<- NULL
          }
          get <- function() x ## this will return the matrix
          setInverse <- function(inverse){ inv <<- inverse} ##this load the matrix 
          getInverse <- function() {inv}
          list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     inv <- x$getInverse()     ## Getting inverse from cache
     if(!is.null(inv)){        ## this will execute if inverse is already present in cache
       message("getting inverse from cache ...")
       return(inv)
     }
     mat <- x$get() # load the matrix
     inv <- solve(mat, ...) # inverting mat
     x$setInverse(inv)
     inv
}
