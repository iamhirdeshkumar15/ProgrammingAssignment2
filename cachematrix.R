## Put comments here that give an overall description of what your
## functions do
#For large square matrices, it may take too long to compute the inverse, especially if it has to be computed repeatedly.
#If the contents of the matrix do not change, it may make sense to cache the matrix inverse so that, when we need it again, it can be looked up in the cache rather than recomputed

## Write a short comment describing this function
#we take advantage of the scoping rules of the R language and observe how they can be manipulated to preserve state inside of an R object.

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
set <- function(y){
  x <<- y
  inv <<- NULL
}
get <- function() x
setinverse <- function(inverse) inv <<- inverse
getinverse <- function() inv
list(set = set, get = get,
     setinverse = setinverse,
     getinverse = getinverse)
}





## Write a short comment describing this function
#This function calculates the inverse of special matrix .
#It first checks to see if the inverse already or not . if yes, then it gets the inverse from the cache and skip the computation.
#otherwise it calculates the inverse of matrix and sets it in the cache  via the setinverse function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
inv <- x$getinverse()
if(!is.null(inv)) {
  message("getting cached data")
  return(inv)
  }
matrix_to_invert <- x$get()
inv <- solve(matrix_to_invert, ...)
x$setinverse(inv)
inv
}
 
# First Example to test my code 
my_Matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
my_Matrix$get()
my_Matrix$getinverse()
cacheSolve(my_Matrix)

#Second Example 
my_Matrix$set(matrix(c(2, 2, 1, 4), 2, 2))
my_Matrix$get()

my_Matrix$getinverse()
cacheSolve(my_Matrix)
my_Matrix$getinverse()




