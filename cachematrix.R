# Matrix inversion is usually a costly computation and there may be some benefit to caching 
# the inverse of a matrix rather than compute it repeatedly 
# (there are also alternatives to matrix inversion that we will not discuss here). 
# Your assignment is to write a pair of functions that cache the inverse of a matrix.

# Write the following functions:
  
# 1. makeCacheMatrix: This function creates a special "matrix" object that can cache 
#    its inverse.
# 2. cacheSolve: This function computes the inverse of the special "matrix" 
#    returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the inverse value of the matrix
# 4. get the inverse value of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, 
       setinverse=setinverse, 
       getinverse=getinverse)
}

# cacheSolve returns the inverse of a matrix.
# if the inverse exists then it returns the cached data.
# if not, it calculates the inverse using the solve() function.

# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

# 2x2 Example:
#> x = rbind(c(1, 2), c(3, 4))
#> m = makeCacheMatrix(x)
#> m$get()
#[,1] [,2]
#[1,]    1    2
#[2,]    3    4
# First run (no cache)
#> cacheSolve(m)
#[,1] [,2]
#[1,] -2.0  1.0
#[2,]  1.5 -0.5
# Second run (pulling out from cache)
#> cacheSolve(m)
#getting cached data.
#[,1] [,2]
#[1,] -2.0  1.0
#[2,]  1.5 -0.5


# 3x3 Example:
#> x = rbind(c(3, 2, 1), c(4, 5, 6), c(9, 8, 6))
#> m = makeCacheMatrix(x)
#> m$get()
#[,1] [,2] [,3]
#[1,]    3    2    1
#[2,]    4    5    6
#[3,]    9    8    6
# First run (no cache)
#> cacheSolve(m)
#[,1]       [,2] [,3]
#[1,]  2.571429  0.5714286   -1
#[2,] -4.285714 -1.2857143    2
#[3,]  1.857143  0.8571429   -1
# Second run (pulling out from cache)
#> cacheSolve(m)
#getting cached data.
#[,1]       [,2] [,3]
#[1,]  2.571429  0.5714286   -1
#[2,] -4.285714 -1.2857143    2
#[3,]  1.857143  0.8571429   -1


