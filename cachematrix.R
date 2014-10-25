## A pair of functions that cache the inverse of a matrix

## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(s = matrix()) {

## Initialize the inverse property
   j <- NULL
   
   ## Method to set the matrix
   set <- function( matrix ) {
       s <<- matrix
       j <<- NULL
    }
    
    ## Method the get the matrix
    get <- function() {
    	## Return the matrix
    	s
    }

    ## Method to set the inverse of the matrix
    setInverse <- function(inverse) {
        j <<- inverse
    }

    ## Method to get the inverse of the matrix
    getInverse <- function() {
        ## Return the inverse property
        j
    }

    ## Return a list of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {

    ## Return a matrix that is the inverse of 'x'
    s <- x$getInverse()

    ## Just return the inverse if its already set
    if( !is.null(s) ) {
            message("getting cached data")
            return(s)
    }

    ## Get the matrix from our object
    data <- x$get()

    ## Calculate the inverse using matrix multiplication
    s <- solve(data) %*% data

    ## Set the inverse to the object
    x$setInverse(s)

    ## Return the matrix
    s
}
