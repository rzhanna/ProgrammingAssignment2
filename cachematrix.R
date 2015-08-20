## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      
      ##Creating the component functions
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      
      get <- function() {
            x
      }
      setinverse <- function(inverse) {
            inv <<- inverse
      }
      getinverse <- function() {
            inv
      }
      ##Packaging the functions into a list
      list(
            set = set,
            get = get,
            setinverse = setinverse,
            getinverse = getinverse
      )
}


## Write a short comment describing this function

cacheSolve <- function(mcm, ...) {
       ## Return a matrix that is the inverse of 'x'
      inv <- mcm$getinverse()
      if (!is.null(inv)) {
            message("getting cached inverse data")
            return(inv)
      } else {
            matrix <- mcm$get()
            inv <- solve(matrix,...)
            mcm$setinverse(inv)
            inv
      }
}
