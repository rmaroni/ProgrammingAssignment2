## AIM: finding a less computationally burdening way to get the inverse
##      of a matrix.
## HOW: by caching the value of the inverse, when that value is available.
##      Alternatively, by calculating the inverse and caching it
##      so that it remains available for later.
## NOTE: two functions defined.

## 1) The function below returns a list of functions:
##      - 'set', to set the values of a matrix
##      - 'get', to return the values of a matrix
##      - 'setinv', to set the values of the inverse of the matrix defined above
##      - 'getinv', to return the values of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {                        # sets values of matrix
        x <<- y
        inv <<- NULL
    }
    get <- function() x                         # retrieves matrix
    setinv <- function(invs) inv <<- invs       # sets values of inverse
    getinv <- function() inv                    # retrieves inverse
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## 2) This function retrieves the cached value of the inverse of the matrix,
##      if available.
##      Otherwise, it retrieves the matrix, calculates its inverse and then
##      caches it.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()                       # retrieves inverse
    if(!is.null(inv)) {                     # if inverse is available:
        message("getting cached data")       ## returns cached data
        return(inv)
    }
    matr <- x$get()                         # else: retrieves matrix
    inv <- solve(matr)                      # calculates inverse
                                             ## (assumption: invertible matrix)
    x$setinv(inv)                           # caches inverse
    inv                                     # returns inverse
}