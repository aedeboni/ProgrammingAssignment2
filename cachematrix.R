## Create a special Matrix object that can cache an inverse value

## The first function, makeCacheMatrix creates a special Matrix obect, which is
## storing a cached inverse value

makeCacheMatrix <- function(x = matrix()) 
{
     i <- NULL
     set <- function(y) {
          x <<- y
          i <<- NULL
     }
     get <- function() x
     setinverse <- function(inverse) i <<- inverse
     getinverse <- function() i
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## The following function calculates the inverse of the custom Matrix object created 
## with the above makeCacheMatrix.  It first checks to see if the inverse has 
## already been calculated. If so, it gets the inverse from the cache and skips the computation.

cacheSolve <- function(x, ...) 
{
     i <- x$getinverse()
     if(!is.null(i)) 
     {
          message("getting cached data")
          return(i)
     }
     data <- x$get()
     i <- solve(data, ...)
     x$setinverse(i)
     i
}
