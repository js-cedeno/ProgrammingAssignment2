## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) { #set i and x values
                x <<- y
                m <<- NULL
  }
        get <- function() x #get matrix
        setinverse <- function(inverse) m <<- inverse #set the inverse matrix
        getinverse <- function() m #Get the inverse matrix
        list(set = set, get = get,setinverse = setinverse,getinverse = getinverse) #Returns a list with the matrix and its inverse.
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {#Can get other paramethers to use in the solve function
        m <- x$getinverse() #Intenta obtener la inversa
        if (!is.null(m)) {
                 message("Getting cached data")
                 return(m)
        }
        data <- x$get() #Get the get (haha) of the makeCacheMatrix
        m <- solve(data, ...) 
        x$setinverse(m)
        m #Deveulve la matrix inversa

}
