## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix:
## PURPOSE: Maintain a variable and its important attributes

## Inputs:datatype - matrix

## Outputs: list of functions - 
## 1). get - returns inverse matrix result
## 2). set - generates inverse matrix value
## 3). setmatrix - populates the matrix value of the variable
## 4). getmatrix - returns the matrix value of the variable

makeCacheMatrix <- function(x = matrix()) {
## ensure the local scope variable m is empty
## m is the value of the inverse matrix
        m <- NULL

## setmatrix -
##        1). assigns the matrix to the parent environment value of x
##        2). ensures the parent scope variable m is empty
        setmatrix <- function(y) {
                x <<- y
                m <<- NULL
        }

## get - returns the inverse matrix result
        get <- function() m

## set - attempts to generate the inverse matrix value
##       using the standard matrix multiply binary operator
        set <- function(matrix) m <<- solve(x) %*% x

## getmatrix - simply returns the value of input
        getmatrix <- function() x

## The return value of calling this function is a list of
## the functions described above
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## Write a short comment describing this function

## cacheSolve:
## PURPOSE:
        ## Return the inverse of the matrix generated in makeCacheMatrix$setmatrix()
        ## If the solution is already present (makeCacheMatrix$get()), then
        ## Do not recalculate the inverse, just return the value; otherwise
        ## calculate the inverse and store it in the input function makeCacheMatrix$set().
        ## and then return the result

## Inputs:datatype - list output from makeCacheMatrix

## Outputs: The value of the matrix inverse

cacheSolve <- function(x, ...) {
##  Attempt to populate the local variable m with the cached result
        m <- x$get()[...]
##  If x$get returns a value the m is not null
        if(!is.null(m)) {
##  Echo in the console that the data was already cached
                message("getting cached data")
##  then return the value to the calling environment and exit the function 
                return(m)
        }
##  x$get must have failed to returns a value.  Generate the inverse matrix
##  Populate local variable p with the matrix in the formal parameter x
        p <- x$getmatrix()
##  Generate inverse matrix and populate local variable m with result
        m <- x$set(p)
##  Returns the resulting value of the inverse matrix operation to the calling
## environment
        m
}
