## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly

## For more information refer to the ProgrammingAssignment2 of the R programming course on Coursera


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        # This function sets the value of the matrix
        set <- function(y){
                x <<- y
                m <<- NULL    
        }
        
        # This function gets the value of the matrix 
        get <- function() x
        
        # This function sets the value of the inverse
        setinverse <- function(inverse) m <<- inverse
        
        # This function gets the value of the inverse 
        getinverse <- function() m 
        
        # List of the properties that the function makeCacheMatrix returns
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Assuming that the matrix supplied is always invertible.
        
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
          
}




