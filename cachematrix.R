## These functions make it more computationally efficient to
## calculate the inverse of a matrix by creating a cached matrix
## that can be called instead of re-calculating an inverse

## this function creates a special 'matrix' object that can cache
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
                  m<- NULL
                  #will set x to matrix
                  set <- function(y) {
                      x <<- y
                      m <<- NULL
                  }
                  #returns matrix when get() called
                  get <- function() x
                  setInv <- function(Inv) m <<- Inv
                  getInv <- function() m
                  list(set = set, get = get,
                       setInv = setInv,
                       getInv = getInv)
}

## computes the inverse of the special 'matrix' returned by
## makeCacheMatrix (above). If the inverse has already been 
##calculated, then cacheSolve retrieves the inverse from the 
##cache

cacheSolve <- function(x, ...) {
        ## calls getInv() from makeCacheMatrix
    m <- x$getInv()
    #if m has Inv, then will return it
    if(!is.null(m)) {
        message("getting cached matrix")
        return(m)
    }
    #otherwise, calls get() from makeCacheMatrix and calculates Inv,
    # sets it back in makeCacheMatrix and returns it
    data <- x$get()
    m <- solve(data, ...)
    x$setInv(m)
    m
}
