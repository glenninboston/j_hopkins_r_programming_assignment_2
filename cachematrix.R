# https://github.com/glenninboston/j_hopkins_r_programming_assignment_2.git

# create a matrix of lists containing the functions to get and set 'x' (a matrix) and 'i' (it's inverse)
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL                  # initialize matrix object 'i'
  set <- function(y) {       # here the set() fx assigns the value of x to the parent environment for access in the cacheSolve() fx with the $ operator
    x <<- y
    i <<- NULL               # this resets any existing 'i' to NULL (i.e.clears any existing values of 'i')
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## this function returns a cached inverse matrix or if not available, calculates the inverse directly
cacheSolve <- function(x, ...) {
  i <- x$getinverse()                                         
  if(!is.null(i)) {                                         # if 'i' exists, this returns i
    message("getting cached inverse matrix")
    return(i)
  }                                                         # else if 'i' does not exist, this calculates the inverse matrix
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)                                           # and caches the new inverse matrix
  i
}




##### TEST (from online discussion)
m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
m1

I2 <- matrix(c(1,0,0,1), nrow = 2, ncol = 2)

n1 <- matrix(c(6,2,8,4), nrow = 2, ncol = 2)
n1


myMatrix_object <- makeCacheMatrix(m1)

# should return exactly the matrix n1
cacheSolve(myMatrix_object)