## This is a function factory that can hold data through lexical scoping for the functions to act on.
## In this case a matrix is passed in and cached and the inverse is passed in and cached.
## makeCacheMatrix takes a matrix as a parameter, stores it, initializes a variable to hold the inverse,
## It then creates 4 functions - get and set for the matrix, and get and set for the inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        ## create a variable "inv" that is in this parent function
        ## lexical scoping means that when these functions in the list are called 
        ## they can reference the variable "inv" in the parent frame and have access 
        ## to previously stored data
        ## x is also in the parent frame of the functions in the list since it is a formal
        ## argument for the makeCacheMatrix function
        inv <- NULL
        
        ## setter for the original matrix
        set <- function(y) {
                x <<- y
                ## null out any existing inverse if the matrix is changed
                inv <<- NULL
        }
        
        ##  getter for the original matrix
        get <- function() x
        
        ##  setter for the inverse matrix
        setInverse <- function(i) inv <<- i
        
        ##  getter for the inverse matrix
        getInverse <- function() inv
        
        ##  the list that is returned holding the functions that can be called
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
        
}


## This function uses makeCacheMatrix.  When called it will return the cached inverse 
## matrix that is stored in makeCacheMatrix, or if there is no inverse cached, retrieve the
## original matrix that was stored, solve it to obtain the inverse and cache the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## define local variable and populate with value
        ## returned from the getInverse function
        inv <- x$getInverse()
        
        ## test for not NULL - indicates that the inverse has been previously 
        ## calculated and stored in the variable "inv" in parent frame
        ## (the defining function of x$getInverse() - lexical scoping)
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        ## otherwise the inverse has not been calculated so calculate it and store the value
        ## in the parent frame of these functions for later reference
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
        
}
