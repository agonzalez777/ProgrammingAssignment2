## Put comments here that give an overall description of what your
## functions do

#makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # By default m (Inversed Matrix) is NULL
  m <- NULL
  # Set assigns the matrix to the special matrix object and the Inversed Matrix is NULL in the makeCacheMatrix 
  # Envrionment. It doesn't solve it by default unless you call the cacheSolve function.
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # Get returns the matrix 
  get <- function() x
  
  # setsolve assigns the inverse matrix to the special matrix objectin the makeCacheMatrix environment
  setsolve <- function(solve) m <<- solve
  
  # returns the inverse of the matrix
  getsolve <- function() m
  
  # provides the set of functions this makeCacheMatrix object exposes through a list
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  # checks to see if the matrix passed has been inversed already. If it has, then it returns the cached solution
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # If not, it calculates the Inverse of the Matrix and writes the solution back to the object. So if one wants to inverse the same matrix, it will not do the calculation twice.
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  
  # Returns the inverse
  m
}
