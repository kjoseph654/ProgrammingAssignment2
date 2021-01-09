#These two functions cache an inverse of a matrix and returns it


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) { #sets the values of the matrix
    x <<- y #caching
    inv <<- NULL #caching
  }
  get <- function() x #gets the values of the matrix
  setinverse <- function(inverse) inv <<- inverse #sets the matrix inverse
  getinverse <- function() inv #gets the matrix inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) #lists the functions
}

#checks to see if inverse has already been created in cache
#if not, then computes inverse
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}