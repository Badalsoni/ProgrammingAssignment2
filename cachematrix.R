## Put comments here that give an overall description of what your
## functions do
## The main motto of this assignment is to familiarize use more about functions. 
## we are supposed to write inverse of cache "makeCachematrix" and "cachSsolve"
## Write a short comment describing this function
## "makeSolve" is basically a function that computes the inverse of a special matrix returned by "makeCachematrix".
## If the inverse of matrix is already being computed then the cache is supposed to return
## inverse from the cache.

makeCacheMatrix <- function(x = matrix()) {
## returns matrix i.e. the inverse of 'x'
        inv <- NULL
        Set <- function(y){
                x <<- y
                inv <<- NULL
}
get <- function(y) {
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get=get, setinv = setinv, getinv = getinv)

## Write a short comment describing this function
## "cacheSolve" is basically the function that computes the inverse of the special matrix returned by
## "makeCachematrix" which is mentioned above and then cahesolve ideally should retrieve inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached output")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
