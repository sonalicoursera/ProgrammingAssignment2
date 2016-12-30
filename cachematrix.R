## using scoping rules of r we are first creating a function that can cache its
## inverse to save computation time of recalculating same thing everything.


## makeCachematrix : This function creates a specil matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, 
       setinv=setinv, 
       getinv=getinv)
}
# creating randon number matrix
s <- matrix(rnorm(24),2,2)
# taking cache
scache <- makeCacheMatrix(s)

## inverse from the cache

cacheSolve <- function(x, ...) {
  inve = x$getinv()
  if (!is.null(inv)){
    message("get cached data")
    return(inv)
  }
  mat.data = x$get()
  inv = solve(mat.data, ...)
  x$setinv(inv)
  return(inv)
}

# testing function
cacheSolve(scache) 

