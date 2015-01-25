# Below are two functions that when used together create a special object that stores a matrix and cache's its inverse.


# Example code to use these functions:
## First, Create a 4x4 matrix
## mat <- matrix(rnorm(16,1,2),4,4)
##
## Second, call makeCacheMatrix
## m <- makeCacheMatrix(mat)
## (Note: this store the matrix in cache)
##
## Next, call cacheSolve
## cacheSolve(m)
##
## Last, subsequent calls to cacheSolve will retrieve the matrix inverse from cache
## cacheSolve(m)


# The first function, makeCacheMatrix creates a special object, which is really a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL # Declare a variable to contain the inverse
        
        # any call to set will store an input matrix in cache and nullify the inverse variable
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        # These 3 are intended to be used by cacheSolve function where:
        # 'm$get' will retrieve the original matrix from cache
        # 'm$setinverse' will store the inverse of the matrix in cache
        # 'm$getinverse' will retrieve the contents of the variable used for the inverse matrix
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        
        # return a list of functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

# The following function calculates the inverse of the special object created with the above function. However, 
# it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and 
# skips the computation. Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the 
# cache via the setinverse function.

cacheSolve <- function(x, ...) {
        
        # retrieve the contents of the variable used to store the inverse matrix within makeCacheMatrix
        m <- x$getinverse()
        
        # If the content is not null, the inverse has been calculated.  Write a message and return the inverse from cache
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        # retrieve the original matrix into 'data'
        data <- x$get()
        
        # calculate the inverse
        m <- solve(data, ...)
        
        # update 'setinverse reference (the function in makeCacheMatrix) to the inversed matrix.  This could be logically
        # interpretted as 'update the cache reference for the inverse matrix location'.
        x$setinverse(m)
        
        # return 'm' which is the inverse matrix solved within this function
        m
}