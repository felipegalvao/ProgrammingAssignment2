## These two functions will create an object representing a matrix
## and will compute a matrix inverse if it's was not calculated
## before or will return the previously calculated inverse

# Function to create a matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  # Set its inverse as null on the creation
  inv <- NULL
  # Function to set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # Function to get the matrix
  get <- function() x
  # Function to set the inverse of the matrix
  setInverse <- function(inverse) m <<- inverse
  # Function to set the inverse of the matrix
  getInverse <- function() inv
  # Returned list
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


# Function to calculate the inverse or get the cached one
cacheSolve <- function(x, ...) {
  # Getting the inverse from the matrix created with the first function
  inv <- x$getInverse()
  # If the inverse is not null, return the cached inverse
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # If the inverse is null, the following code will get the matrix
  data <- x$get()
  # Solve function to calculate the inverse matrix
  inv <- solve(data, ...)
  # Set the inverse of the matrix
  x$setInverse(inv)
  # And return the inverse matrix
  inv
}
