##
## The script contains two functions that facilitate caching 
## pre-calculated inverse of a matrix.
## 
## makeCacheMatrix - creates a special matrix "object", encapsulating
##                   data (matrix) and its inverse. It returns functions 
##                   to manipulate data and cached inverse.
## 
## cacheSolve      - a function that returns an inverse of a special
##                   matrix "object" given as input. It only computes
##                   the inverse if it has not been yet computed for the
##                   given input data.



## makeCacheMatrix
##  INPUT:  matrix object to store in function's environment
##  OUTPUT: a list of 4 functions to manipulate the original matrix and its
##          inverse: get, set, setInverse, getInverse
##             
##  The logic simulates object-oriented programming :)

makeCacheMatrix <- function(x = matrix()) {
      
    ## on init we set the inverse to NULL
    inv <- NULL
    
    ## Set the data (x) and invalidate
    ## the cached inverse (inv).
    ## Do it in parent's environment.
    set <- function( y ) {
        x   <<- y
        inv <<- NULL
    }
    
    ## Just return the original matrix
    get <- function() {
        x
    }
    
    ## Set the inverse
    setInverse <- function( i ) {
        inv <<- i
    }
    
    ## Return the inverse
    getInverse <- function() {
        inv
    }

    ## The output value is a list of four data manipulation functions
    list( set=set, get=get, setInverse=setInverse, getInverse=getInverse)

}


## cacheSolve
##   INPUT:   the special matrix "object", as returned 
##            by makeCacheMatrix function
##   OUTPUT:  the inverted matrix
##
## This function only solves the matrix if it hasn't been 
## done so far. In case the cached inverse is available (i.e. not null),
## it is returned directly without re-solving.
## 

cacheSolve <- function(x, ...) {
    # retrieve cached inverse
    inv <- x$getInverse()
    
    # If inverse found (i.e. not null), return it
    # otherwise, calculate it and store within a matrix "object"
    if ( !is.null(inv) ) {
        message("getting cached data")
        return(inv)
    } else {
        # get matrix
        mtx <- x$get()
        # invert it
        inv <- solve(mtx)
        # store the inverse in cache
        x$setInverse(inv)
        inv
    }
}
