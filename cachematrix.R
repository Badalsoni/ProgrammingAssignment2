## Put comments here that give an overall description of what your
## functions do
## The main motto of this assignment is to familiarize use more about functions. 
## we are supposed to write inverse of cache "makeCachematrix" and "cachSsolve"
## Write a short comment describing this function
## "makeSolve" is basically a function that computes the inverse of a special matrix returned by "makeCachematrix".
## If the inverse of matrix is already being computed then the cache is supposed to return
## inverse from the cache.

makeCacheMatrix <- function(x = matrix()) {

inv <- NULL
set <- function(y) {
x <<- y
inv <<- NULL
}
get <- function() x
setinv <- function(inverse) inv <<- inverse
getinv <- function() inv
list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Write a short comment describing this function
## "cacheSolve" is basically the function that computes the inverse of the special matrix returned by
## "makeCachematrix" which is mentioned above and then cahesolve ideally should retrieve inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached result")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
        
## ---------------Checking the program-----------------------
 m <- matrix(rnorm(16),4,4)
 m1 <- makeCacheMatrix(m)
cacheSolve(m1)

##           [,1]       [,2]       [,3]       [,4]
##[1,]  1.7107659  0.5705967 -1.0597085 -0.7598147
##[2,] -4.2885292 -1.5347577  3.5950312  4.0708751
##[3,]  1.5342610  0.7168858 -1.6785695 -1.2112209
##[4,]  0.3030741 -0.8524399  0.0189958  0.1534455
