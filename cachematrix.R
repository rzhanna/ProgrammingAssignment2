## In general, the makeCacheMatrix function for storing a matrix as well as a 
## list of 'special functions' associated with setting & retrieving the 
## stored matrix and setting & retrieving the matrix's invserse.

## The cacheSolve() function takes the makeCacheMatrix() function/list as an 
## input* and will yield either a cached value of the matrix's inverse
## (if it was set beforehand), or will calculate the inverse, cache it, and 
## return the inverse as an output.

## *Note: This assumes the makeCacheMatrix was called initially with a matrix 
## (e.g. >> makeCacheMatrix((matrix(1:3,nrow = 3)))), or the the stored matrix 
## was set at a later time using the $set() function.

## -----------------------------------------------------------------------------

## The makeCacheMatrix takes a matrix as a formal argument, stores it, and 
## creates a list of functions $set, $get, $setinverse, $getinverse for 
## subsequently getting the matrix, setting/re-setting the matrix, setting a 
## a cached version for the matrix's invserse , and then retrieving that cached 
## inverse, respectively.

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

## The cacheSolve() function takes as an input the makeCacheMatrix() function/
## list of functions (refered in the function as "mcm") and returns a matrix 
## that is the INVERSE of the one stored in the makeCacheMatrix().

cacheSolve <- function(mcm, ...) {
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