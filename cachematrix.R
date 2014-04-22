## The function makeCacheMatrix() creates a special "vector"
## which is really a list containing a function to
##  1. set the value of the matrix
##  2. get the value of the matrix
##  3. set the inverse of the matrix
##  4. get the inverse of the matrix
##
## actually it is almost the same as the sample in the assignment


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        ## creating the functions
        ## set a new matrix & reset the value of the inverse
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## get the input matrix
        get <- function() x
        
        ## set the inverse matrix
        setinverse <- function(inv) m <<- inv
        
        ## get the inverse matrix
        getinverse <- function() m
        
        ## creating the output list of functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}



## The function cacheSolve() calculates the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getinverse()
        
        ## check if the matrix already inverted
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## OK, it's a matrix...
        data <- x$get()
        
        ## check if the input is a matrix
        if (!is.matrix(data)) stop("not a matrix")
        
        ## try to invert with error handling
        m <- try(solve(data, ...),TRUE)
        
        ## check if the matrix is invertible
        if (class(m)!="matrix") stop("matrix is not invertible")
        
        ## it's an invertible matrix
        x$setinverse(m)
        m
}


## Usage of the functions:

## init a new matrix eg.:
##
## > a <- makeCacheMatrix(matrix(c(2,5,-6,7,3,9,-2,1,4),3,3))
## > a$get()
##
## calculate the inverse-matrix:
##
## > cacheSolve(a)
## _______________________________________________________
##
## set a new matrix:
## > a$set(matrix(c(2,-6,7,9),2,2))
##
## after setting a new matrix, the inverse-matrix must re-calculated:
## > cacheSolve(a)
##
## let's check it:
## > a$get()
## > a$getinverse()