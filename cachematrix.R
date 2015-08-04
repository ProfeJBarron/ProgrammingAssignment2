## Here we have a pair of functions that are designed to compute the inverse
## of a matrix in a cached way to optimize the time.

## This functions creates a "special matrix" to be able to cached the inverse
## of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
  
}


## This function receives a "special matrix" and search for the cached inverse
## if not exists then is computed and cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
    i <- x$getInverse()
    if(!is.null(i)) {
      message("getting cached inverse matrix")
      return(i)
    }
    m <- x$get()
    i <- solve(m)
    x$setInverse(i)
    i

}

## To test functions do:
## 1-create a square matrix, say "mm" (be sure it has inverse)
## 2- execute ">sm <- makeCacheMatrix(mm)" to get the special matrix
## 3-execute ">cacheSolve(sm)" to compute the inverse
## 4-execute again ">cacheSolve(sm)" to get the cached inverse


