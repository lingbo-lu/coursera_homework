## Function that set the x value, get the x value
#set the inverse value, get the inverse value,
#and returns a list of the functions described above

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  #an enclosed function to set x value with y
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  #an enclosed function that simply returns the x value
  get <- function() x
  #an enclosed function that gives m the value of 
  #the calculated inverse
  setinverse <- function(inverse) m <<- inverse
  #an enclosed function that returns the m value
  getinverse <- function() m
  #return a list of functions defined above
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function that accepts a makeCacheMatrix object, takes
#the cached x value, checks if its inverse has been calculated,
#and returns the cached inverse value if available, or
#calculate it and cache it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  #get inverse and check if it's been calculated
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  #else, calculate the inverse and cache that
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  #return the calculated result
  m
}
