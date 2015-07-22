# makeCacheMatrix creates a specail "matrix", which is really a list containing a function to:
# 1. set          set the value of a matrix
# 2. get          get the value of a matrix
# 3. setinverse   set the cahced value (inverse of the matrix)
# 4. getinverse   get the cahced value (inverse of the matrix)

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() {
            x
      }
      setinverse <- function(solve) {
            m <<- solve
      }
      getinverse <- function() {
            m
      }
      list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# cacheSolve calculates the inverse of the "special" matrix created with makeCacheMatrix.
# It first checks to see if the inverse has already been calculated.
# If so, it gets the inverse from the cache.
# Otherwise, it calculuates the inverse and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
      m <- x$getinverse()
      if(!is.null(m)){
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data,...)
      x$setinverse(m)
      m
}