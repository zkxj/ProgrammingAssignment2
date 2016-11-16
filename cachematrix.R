## The enclosed functions are intended to streamline matrix
## inversion process by caching the inversion results as they are
## computed. The goal is to improve computation times for the
## "solve" function in R by avoiding the computation if possible.
##
## 2 functions are included:
##      1. makeCacheMatrix:
##          Create a special matrix object that can store both its
##          "straight" (original) and inverse values
##      2. cacheSolve:
##          Compute the inverse value of a matrix. If the computation
##          has already been done, return the cached inverse value

## Comment Credit: RDPeng
## This function creates a special "matrix" object that can
## cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  ## Start off with a null inverse value
  mInverse <- NULL
  
  ## Set the new "straight" matrix value
  setStraight <- function(mStraight) {
    x <<- mStraight         ## Set the new straight matrix value
    
    ## Since a new straight matrix was created, clear the cached 
    ## matrix value
    mInverse <<- NULL
  }
  
  getStraight <- function() x     ## Retrieve straight matrix values
  
  ## Create the inverse value
  setInverse <- function(invert) mInverse <<- invert
  
  ## Get the inverse value
  getInverse <- function() mInverse
  
  ## Return a list of functions available for our custom Matrix object
  list(setStraight = setStraight, getStraight = getStraight,
       setInverse = setInverse, getInverse = getInverse)
}


## Comment Credit: RDPeng
## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then the
## cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'. By default, the
  ## cached value is returned
  mInverse <- x$getInverse
  
  if (!is.null(mInverse)) { ## If the cached inverse value IS NOT null
    
    ## Notify that a cached value is returned
    message("Inverse already computed. Returning cached value")
    return(mInverse) ## Return the cached value
  } else {    ## If the cached value IS null
    mStraight <- x$getStraight()    ## Get the straight matrix value
    mInverse <- solve(mStraight, ...) ## Compute inverse of the matrix
    
    ## Set the cached inverse value for future use
    x$setInverse(mInverse)
    mInverse    ## Finally, return the inverse value of the matrix
  }
}


