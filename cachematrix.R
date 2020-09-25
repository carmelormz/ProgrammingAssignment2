## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        ## Set data cache to NULL
        invMatrix <- NULL
        
        ## Setter method
        set <- function(m) {
          x <<- m
          invMatrix <<- NULL
        }
        
        ##Getter Method
        get <- function() x
        
        ## Setter method for cache data
        setInvMatrix <- function(m) invMatrix <<- m
        
        ## Cache method for cache data
        getInvMatrix <- function() invMatrix
        
        ## Return special matrix
        list(set = set,
             get = get,
             setInvMatrix = setInvMatrix,
             getInvMatrix = getInvMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## Check if there is cache data
        invMat <- x$getInvMatrix();
        
        if (!is.null(invMat)) {
          message("getting cached data...")
          return(invMat)
        }
        
        ## Get matrix from special matrix
        dataMat <- x$get()
        
        ## Calculate inverse matrix
        result <- solve(dataMat)
        
        ## Set cache data
        x$setInvMatrix(result)
        
        ## Return result matrix
        result
}
