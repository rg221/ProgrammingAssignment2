########################################################################
## cachematrix.R  -  Programming Assignment 2: Lexical Scoping 
##
## This file contains 2 function that support inverting a matrix:
##  - makeCacheMatrix - This function creates a special "matrix" 
##                      object that can cache its inverse.
##  - cacheSolve      - This function computes the inverse of the
##                      special "matrix" object returned by the
##                      makeCacheMatrix function.  If the inverse
##                      has already been calculated and the matrix
##                      has not changed, then the function will
##                      return the cached value, otherwise the
##                      matrix inverse will be calcualted and returned.
##
########################################################################

makeCacheMatrix <- function(x = matrix()) {
   #####################################################################
   ## Function: makeCacheMatrix
   ##   
   ## This function will create a special "matrix" object that supports
   ## computing and caching the inverse of a caller-supplied matrix.
   ##
   ## Input Parameters:
   ##   x = Input matrix, assumed to be square.
   ##
   ## Assumptions:
   ##   - input matrix is an invertable matrix.
   ##   - There is no validation of input parameters.
   #####################################################################

   ## First step is to create a "null" inverse matrix.  This will be 
   ## properly computed later.
   x.inverse <- NULL
   
   ## Define functions that will be saved in our "special" output matrix.
   
   ## Define: set - Save local cached copies of the matrix and its inverse.
   set <- function(y) {
            ## If the caller supplied matrix y is identical to what we have
            ## saved (x), then there is nothing to do.  Otherwise initialize
            ## for a new matrix.
            if ( !( ( is.matrix(x) && is.matrix(y) ) &&
                    ( dim(x) == dim(y)             ) &&
                    ( all(x == y)                  )    ) ) {
               x         <<- y     # Save input matrix
               x.inverse <<- NULL  # Inverse not yet computed
            }
          }   
   ## Define: get - Return a copy of our original matrix
   get        <- function() x
   ## Define: setinverse - Save matrix inverse (in local cache)
   setinverse <- function(inverse) x.inverse <<- inverse
   ## Define: getinverse - Get current matrix inverse
   getinverse <- function() x.inverse

   ## Create the output "special" matrix that will be returned
   ## to the funtion caller.  This "special" matrix will be used
   ## with the cacheSolve() function to comput the matrix's inverse.
   
   list(set = set, 
        get = get,
        setinverse = setinverse,
        getinverse = getinverse)
   
}  ## END - makeCacheMatrix


cacheSolve <- function(x, ...) {
   #####################################################################
   ## Function: cacheSolve
   ##   
   ## The cacheSolve() function works with the makeCacheMatrix()
   ## function to compute and return the inverse for a "special"
   ## matrix object (which was created by makeCacheMatrix).
   ##
   ## Input Parameters:
   ##   x = "Special" matrix object created by makeCacheMatrix().
   ##
   #####################################################################
   
   ## First step is to see if we have a cached copy of our
   ## matrix inverse.
   x.inverse <- x$getinverse()
   if(!is.null(x.inverse)) {
      ## Good news - we have a cached copy of the inverse.
      ## Return the cached copy to the caller.
      message("cacheSolve - using cached copy of inverse")
      return(x.inverse)
   }
   
   ## No cached copy of the matrix inverse.  Do the following:
   ##   (1) Compute the inverse
   ##   (2) Save the inverse in our local cache
   data <- x$get()
   x.inverse <- solve(data,diag(1,nrow=nrow(data),ncol=nrow(data)))
   x$setinverse(x.inverse)
   
   ## Return a matrix that is the inverse of 'x'
   x.inverse
   
}  ## END - cacheSolve

