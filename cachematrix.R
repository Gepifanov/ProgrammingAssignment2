# Create cache inverse matrix.
makeCacheMatrix <- function(x = matrix()) {
  inv_mat <- NULL
  set <- function(y) {
    x <<- y
    inv_mat <<- NULL
  }
  get <- function() x
  Set_Inverse <- function(inverse) inv_mat <<- inverse
  Get_Inverse <- function() inv_mat
  list(set = set,
       get = get,
       Set_Inverse = Set_Inverse,
       Get_Inverse = Get_Inverse)
}

# Get inverse matrix from cache (if we already calculated it) or calculate inverse matrix
cacheSolve <- function(x, ...) {
  inv_mat <- x$Get_Inverse()
  if (!is.null(inv_mat)) {
    message("Inverse Matrix is already calculated... I am getting cached")
    return(inv_mat)
  }
  mat <- x$get()
  inv_mat <- solve(mat, ...)
  x$Set_Inverse(inv_mat)
  inv_mat
}
