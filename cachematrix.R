## makeMatrix and cacheinverse can be used together to 
## store the value of a matrix, cache its inverse, and 
## store the value of the cached inverse

##makeMatrix takes the matrix inupt, and creates variables
## for set, get, setinverse, and getinverse to be used in the 
## next function
makeMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() 
            x
      setinverse <- function(inverse) 
            inv <<- inverse
      getinverse <- function() 
            inv
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

##cacheinverse takes the list of variables created in the first function
## and calls get inverse to determine if the inverse has already been taken
## on the matrix.  If not, it "get"s the matrix and inverts in using the solve
##function.  The inverse is then stored using setinverse and the inverse value
## "inv" is returned.
cacheinverse <- function(x, ...) {
      inv <- x$getinverse()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinverse(inv)
      inv
}