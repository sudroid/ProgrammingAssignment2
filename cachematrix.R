## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        cacheMatrix <- NULL
        
        setMatrix <- function(y) {
                x <<- y
                cacheMatrix <<- NULL
        }
        getMatrix <- function() cacheMatrix
        
        setInverse <- function(invsereMatrix) cacheMatrix <<- invsereMatrix
        
        getInverse <- function() cacheMatrix
        
        list(setMatrix = setMatrix, 
             getMatrix = getMatrix, 
             setInverse = setInverse, 
             getMatrix = getMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        rMatrix <- x$getInverse
        if(!isnull(rMatrix)) {
                message("getting cache data")
                return(rMatrix)
        }
        
        data <- x$getMatrix()
        rMatrix <- solve(data)
        x$setInverse(rMatrix)
        ## Return a matrix that is the inverse of 'x'
        rMatrix
}
