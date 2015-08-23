## MakeCacheMatrix() and cacheSolve ()functions togather calcultates and stores the inverse of a matrix in memory.
## It reduces computation by storing inverse of a matrix.

## Write a short comment describing this function  MakeCacheMatrix() creates list of simple functions that can be performed on Matirx. The list includes get(), set(), setmatrix() and getmatrix(). 

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
  set <- function(y)
   { x <<- y
    m <<- NULL
    }
  get <- function() x   ## gets a matrix if stored previously.
  setmatrix <- function(inverse) m <<- inverse  ## change a inverse of matrix stored in memory of to store  value of a matrix
  getmatrix <- function() m    ## obtains a previously stored matrix
  list( set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)

}


## cacheSolve() function checks if the inverse of input matrix is previously stored in memory. 
##If yes, then returns it. If no, then calculates inverse of the input matrix and stores it into memory.

cacheSolve <- function(x, ...) {
   m <- x$getmatrix()
  if(!is.null(m)) { message (" getting cached data")
    return (m)}
  data <- x$get()
  m <- solve(data,...)
  x$setmatrix(m)
  m                                 ## Return a matrix that is the inverse of 'x'
}
