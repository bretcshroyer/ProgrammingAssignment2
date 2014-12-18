## For R Programming Course, week2
## Create two functions: 
##  one which creates methods extending the second
##  and one which will return the (cached, if previously calculated) inverse
##  of a matrix


## creates object methods for the matrix in question
## usage:  myMatrix<-makeCacheMatrix(data)
## myMatrix object has $set, #get, $setinv, $getinv methods

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) m <<- inverse
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## return the inverse of a matrix object created by makeCacheMatrix function
## if inverse has already been calculated, return the inverse from cache
## if not, calculate inverse and cache it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached inverse data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}

## test code
B = matrix(c(2, 4, 3, 1,5,2,6,1,9), nrow=3, ncol=3)

BB<-makeCacheMatrix(B)
cacheSolve(BB)
cacheSolve(BB)
