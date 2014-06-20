# makeCacheMatrix: return a list of functions to:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  # inv will store the cached inverse matrix
  inv <- NULL
  
  # Setter for the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # Getter for the matrix
  get <- function() x
  
  # Setter for the inverse
  setinv <- function(inverse) inv <<- inverse
  
  # Getter for the inverse
  getinv <- function() inv
  
  # Return the matrix with our newly defined functions
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


# cacheSolve: Compute the inverse of the matrix. If the inv variable is already 
# exist's in the environment, then return the inv from cached variable.
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  
  # If the variable inv is already calculated, return it
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # The inv not found in the enviroment and calculate it using the solve()
  data <- x$get()
  inv <- solve(data, ...)
  
  # Cache the inv varaible
  x$setinv(inv)
  
  # Return the value of inv
  inv
}

# To Test above functions(submit one line at a time):

## matrix <- matrix(rnorm(9), 3, 3)                   // Create 3X3 matrix
## c_inv_m <- makeCacheMatrix(matrix)           // Create our special matrix
## m <- c_inv_m$get()                          // Get the matrix
## inv_m <- cacheSolve(c_inv_m)                // Get the inverse matrix
## inv_m1 <- cacheSolve(c_inv_m)               // Call the function againt to see 
##                                             // the cached inverse (same as inv_m)
## round(inv_m %*% m, 3)                       // multiply matrix data with inverse
