## Put comments here that give an overall description of what your
## functions do

## This functions takes a matrix as input and stores it in a different environment 
#so that when it's called again it will not calculate it every time

makeCacheMatrix <- function(x = matrix()) {
      Inverse <- NULL
      
      ## Create list of functions that will set or get the inverse
      list ( 
            set = function (x_set) {
                  x <<- x_set
                  Inverse <<-NULL
            },
            
            get = function () x, #Gets the called matrix
            setInverse = function (inverse_local) Inverse <<- inverse_local,
            getInverse = function () Inverse
            
      )
}



## This function solves the inverse of the matrix while checking if it had already been solved and stored

cacheSolve <- function(x,...) {
      
      Inverse <- x$getInverse()
      
      if (!is.null(Inverse)) {
            message ("Use inverse in cache")
            return (Inverse)
      }
      
      mat <- x$get()
      Inverse <- solve (mat,...)
      x$setInverse (Inverse)
      
      Inverse
}