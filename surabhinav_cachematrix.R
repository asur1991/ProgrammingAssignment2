## This code here will write two functions that caches the inverse of a matrix ((considering that the matrix is invertible)
## The first function creates a special matrix that can cache its inverse
## The second function computes the inverse of the special matrix. If the inverse has already been computed, then this function will retrieve the inverse 
## from the cache

## Function 1: Creating a special matrix to cache its inverse

makeCacheMatrix <- function(x = matrix()) {

	## Initialize the inverse property
    inv <- NULL

    ## Method to set the matrix
    set <- function(y) {
            x <<- y
            inv <<- NULL
  }

    ## Method the get the matrix
    get <- function() {
    	## Return the matrix
    	x
    }

    ## Method to set the inverse of the matrix
    setInverse <- function() 
    inv <<- solve(x) #calculate the inverse
    getInverse <- function() 
     inv

    ## Return a list of the methods
    list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Function 2: Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()

    ## Just return the inverse if its already set
    if(!is.null(m)) {
            message("getting cached data")
            return(m)
    }

    ## Get the matrix from our object
    data <- x$get()

    ## Calculate the inverse using matrix multiplication
    m <- solve(data) %*% data

    ## Set the inverse to the object
    x$setInverse(m)

    ## Return the matrix
    m
}




  
    
  
  
  

