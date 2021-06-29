## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  #null initialization
  set <- function(y) {  #method to set the matrix
    x <<- y
    inv <<- NULL
  }
  get <- function() x  #get matrix x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv # return inverse
  list(set = set,  #return method list
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if (!is.null(inv)) { #return matrix if its not null
    message("getting cached data")
    return(inv)
  }
  data <- x$get()  #get matrix
  inv <- solve(data, ...) # solve inverse
  x$setinverse(inv) #set inverse
  inv
}





