## functions that cache the inverse of a matrix

# makeCacheMatrix is a function that returns a list of functions
# it creates a matrix object that can cache its inverse
# makeCacheMatrix contains the following functions: 
        #  setMatrix      set the value of a matrix
        #  getMatrix      get the value of a matrix
        #  setInverse     get the cahced value (inverse of the matrix)
        #  getInverse     get the cahced value (inverse of the matrix)

makeCacheMatrix <- function(x = matrix()) {
        
        # Initializing the inverse of the Matrix 
        matrixInverse <- NULL
        
        # Method to set the matrix
        setMatrix <- function(newValue){
                x <<- newValue
                matrixInverse <<- NULL
        }
        
        # Method to get the matrix
        getMatrix <- function() {
                # returning matrix
                x
        }
        
        # Method to set the inverse of the matrix
        setInverse <- function(inverse) {
                # storing inverse 
                matrixInverse <<- inverse
        }
        
        # Method to get the inverse of the matrix
        getInverse <- function() {
                # returns the inverse
                matrixInverse
        }
        
        # Returns the list of methods
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)
        
}


# cacheSolve computes the inverse of the special matrix returned by the "makeCacheMatrix" function. 
# If the inverse has already been calculated (and the matrix has not changed), then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        # getting the inverse of 'x' matrix
        matrixInverse <- x$getInverse()
        
        # returns if the inverse has already been calculated (i.e. if !is.null(m)==TRUE)
        if(!is.null(matrixInverse)) {
                message("getting cached data")
                return(matrixInverse)
        }
        
        # If the inverse matrix has not yet been calculated     
        # getting the matrix from our object
        data <- x$getMatrix()
        
        # calculating the inverse of the matrix
        inverse <- solve(data) 
        
        # storing the inverse for future usage
        x$setInverse(inverse)
        
        # returning a matrix that is the inverse of 'x'
        inverse 
}
############################################################
# Running Instructions:

# replace the '~/ProgrammingAssignment2/' with the directory you save the file into
# or read the file directly from the web

# source('~/ProgrammingAssignment2/cachematrix.R')

# create a "square" matrix (because `solve` only handles square matrices)
# create the matrix during the call of makeCacheMatrix()
m <- makeCacheMatrix( matrix(c(1,2,11,12), nrow = 2, ncol = 2) );

# Check the output with
m$getMatrix()

# Expected Results:
#            [,1] [,2]
#       [1,]    1   11
#       [2,]    2   12

# Getting the inverse of the matrix
cacheSolve(m)

# Expected Results:
#            [,1] [,2]
#       [1,] -1.2  1.1
#       [2,]  0.2 -0.1

# Getting cached data
cacheSolve(m)

# Expected Results:
#       getting cached data 
#            [,1] [,2]
#       [1,] -1.2  1.1
#       [2,]  0.2 -0.1

# Confront the results with a matrix calculator
# I used this one -> http://www.bluebit.gr/matrix-calculator/calculate.aspx
# Results:

#       Input matrix:
        
#               1.000 11.000
#               2.000 12.000

#       Matrix Inverse:
        
#              -1.200  1.100
#               0.200 -0.100
############################################################


