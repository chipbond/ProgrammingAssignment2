# The following functions perform two tasks:
#
# 1. makeCacheMatrix creates a series of functions 
#    that return a special object (a list of functions)
#    that has the ability to store the inverse of a
#    given matrix "testmatrix" in the cache.
#
# 2. cacheSolve computes the inverse of the given matrix.
#    If the inverse has already been cached then the 
#    function will return the inverse from the cache,
#    otherwise it will calculate and return the inverse
#    while also storing it in the cache.

# This function corresponds to #1 above.  It is composed
# of four functions that are returned as a list that can
# be used to modify the input matrix as well as to retrieve
# the information stored in the cache (both matrix and inverse).

makeCacheMatrix <- function(x = matrix()) { # Opens function and defines inputs
        mat <- NULL # defines the matrix object to be modified by the series of functions
        set <- function(y)  {  # nests a function within makeCacheMatrix that allows you to redefine the input matrix
                x <<- y
                mat <<- NULL
        }
        get <- function() x   # returns the input matrix
        setsolve <- function(solve) mat <<- solve # stores the calculated inverse of the matrix in the cache
        getsolve <- function() mat # returns the inverse of the input matrix
        list(set = set, get = get,  # creates a list with the 4 nested functions defined above
             setsolve = setsolve,
             getsolve = getsolve)
}


# Cache solve does one of two things, it either calculates the value of the inverse for the input matrix
# or it returns the value of the inverse that is already stored in the cache

cacheSolve <- function(x, ...) { # Opens the function and defintes the inputs
        ## Return a matrix that is the inverse of 'x'
        mat <- x$getsolve() # Looks for the inverse of the input matrix in the cache
        if(!is.null(mat)) {  # logical test for whether the inverse is already in the cache
                message("getting cached data") # if the inverse is found in the cache a message is returned
                return(mat) # returns the inverse as it was stored in the cache
        }
        data <- x$get() # if the inverse was not found in the cache, this retrieves the input matrix and assigns it to data
        mat <- solve(data, ...) # defines mat as the inverse of the matrix "data" and calculates the inverse
        x$setsolve(mat) # stores the inverse of the input matrix in the cache if it was not previously stored
        mat # returns the calculated value of the inverse of the input matrix
}

# Below is a test case that can be used to verify the functionality of the above functions

matrix <- matrix(c(1, 2, 3, 11), 2, 2) # Defines a 2 by 2 matrix
testmatrix <- makeCacheMatrix(matrix)  # Defines the input matrix for the functions
cacheSolve(testmatrix) # Checks the cache for a stored inverse value; calculates and returns the inverse if it is not found in the cache
cacheSolve(testmatrix) # Second instance of same function present to show behavior when inverse is found in cache
