## This functions will store a matrix and cache its inverse

## Here we will first set and get the value of the matrix, and then of the inverse

makeCacheMatrix <- function(x = matrix()) {
  imatrix <- NULL
  
  set <- function(y){
    x <<- y
    inv <<- NULL
  }

  get <- function() x
    setInvMat <- function(inverse) imatrix <<- inverse
    getInvMat <- function(){
      imatrix
    }
  list(set = set,
       get = get,
       setInvMat = setInvMat,
       getInvMat = getInvMat)
  
}


## This will calculate the inverse of the matrix returned above

cacheSolve <- function(x, ...) {
  imatrix <- x$getInvMat()
  if (!is.null(imatrix)) {
    message("getting cached data")
    return(imatrix)
  }
  data <- x$get()
  imatrix <- solve(data, ...)
  x$setInvMat(imatrix)
  imatrix
}

