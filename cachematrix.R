## Put comments here that give an overall description of what your
## functions do

## Assumption - The matrix supplied is always invertible

## makeCacheMatrix - makes a list of functions which take the input 
## variable from cacheSolve function. This function itself does not 
## provide the inverse, but saves the values related to each 
## function call for future reference

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv = NULL    # initialize mean variable to NULL
    set = function(y){    # function to set value of vector/matrix.
        x <<- y           #   Not required if you dont want to
        inv <<-NULL       #   change vector on the fly
    }
    get = function() x    # get function to return vector

    # function to set inverse value and assign the same to variable in parent
    #   environement/makeCacheMatrix envrionement
    setInv = function(inverse) inv <<- inverse

    #  Retrive value of inverse
    getInv = function() inv
    
    # Return the set of functions as a list so that they are all 
    #   available to be called in other functions
    list(set = set, get = get, setInv=setInv, getInv=getInv)
}


## cacheSolve - function to cache the inverse of a metrix and return
##   the same, instead of re-calculating, if required
## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv = x$getInv()     # Call getInv to get inverse of the input matrix
    if(!is.null(inv)) {  # If inverse is found, use cache
        message("getting cached result")
        return(inv)
    }

    # if inverse is not yet calculated, continue to calculate the same
    #   and assign the value to the variable which got created in the 
    #   scope of this matrix
    data = x$get()         # get the input matrix
    inv = solve(data)      # get inverse of the input matrix
    x$setInv(inv)          # set the inverse of the input matrix
                           #   to the above calculated value.
    inv                    # Return inverse of matrix
}