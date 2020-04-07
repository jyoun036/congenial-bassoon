## Creates a matrix so that we can cache its inverse.  

cacheMatrix <- function(x = matrix()) 
{
  i <- NULL
  set <- function(y) 
    {
       x <<- y
       i <<- NULL
   }
   get <- function() x
   setinverse <- function(inverse) i <<- inverse
   getinverse <- function() i
   list(set = set,
       get = get,
       setinverse = setinverse, ## set the inverse
       getinverse = getinverse) ## get the inverse
}


## Computer the inverse of a matrix x

cacheSolve <- function(x, ...) 
{
  i <- x$getinverse()
  if (!is.null(i)) ## Has the inverse already been computed?
 {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
