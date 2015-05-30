## This file contains two functions.  The aim of the functions is to reduce the 
## calculation time of calculating the inverse of a matrix by caching the result 
## for future use rather than recalculating the inverse every time

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    
    # Set the inverse to NULL
    inverse <- NULL
    
    # Create the set function that will allow you to overwrite x with a new matrix
    # It sets inverse back to NULL to clear any previously saved inverses to
    # force it to recalculate rather than using the cache
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    # The get function allows you to retrive the value of x
    get <- function() {x}
    
    # This function saves the inverse calculated in cacheSolve to inverse
    setinv <- function(invtosave) {inverse <<- invtosave}
    
    # This function retrives the inverse value
    getinv <- function() {inverse}
    
    # The output of the function is a list of all of the functions created above
    # That can be called later on if this function is assigned to an object
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
} # end makeCacheMatrix


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        
    ## Return a matrix that is the inverse of 'x'
    
    # First see if the inverse has been saved previously in makeCacheMatrix
    inverse <- x$getinv()
    
    # If the value of the inverse is not NULL then return the saved value
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    } # end if
    
    # Else if there inverse does not exist in the cache then
    # Get the matrix data using the get function from makeCacheMatrix
    data <- x$get()
    
    # Calculate the inverse of the matrix
    inverse <- solve(data, ...)
    
    # And save the inverse of the matrix to makeCacheMatrix
    x$setinv(inverse)
    
    # Return the result
    inverse
    
} # end cacheSolve


# Testing inversing a matrix without the functions
# mat <- matrix(c(1,2,3,4), c(2,2))
# mat
# solve(mat)

# Testing that the functions are doing what I wanted
# a <- makeCacheMatrix(mat)
# a$get()
# a$getinv()
# 
# cacheSolve(a)
# cacheSolve(a)
# 
# a$set(matrix(c(2,4,6,8), c(2,2))) 
# a$get()
# cacheSolve(a)
# cacheSolve(a)
# a <- makeCacheMatrix(matrix(c(2,4,6,8), c(2,2)))
# a$get()
# cacheSolve(a)
# cacheSolve(a)
# solve(matrix(c(2,4,6,8), c(2,2)))
