## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates an object which contains a matrix and the inverted one if computed before.
makeCacheMatrix <- function(x = matrix()) {
    matrix_inverse <- NULL;
    set <- function(y) {
      x <<- y;
      matrix_inverse <<- NULL;
    }
    get <- function() x;
    set_inverse <- function(inverse) matrix_inverse <<- inverse;
    get_inverse <- function() matrix_inverse;
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse);
}


## Write a short comment describing this function
## This function returns the inverse matrix of an object created by makeCacheMatrix function.
## Computation of the inverse matrix is based on the solve function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverted <- x$get_inverse();
  if(!is.null(inverted)) {
    message("getting cached data");
    return(inverted);
  }
  tmp_matrix <- x$get();
  tmp_matrix <- solve(tmp_matrix, ...);
  x$set_inverse(tmp_matrix);
  tmp_matrix;
}
