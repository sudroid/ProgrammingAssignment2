## makeCacheMatrix 
## This function will create a matrix object that will 
## cache the inverse of the matrix it is given. A list 
## containing functions to: set and get the matrix and
## set and get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        cacheMatrix <- NULL
        
        setMatrix <- function(y) {
                x <<- y
                cacheMatrix <<- NULL
        }
        getMatrix <- function() cacheMatrix
        
        setInverse <- function(invsereMatrix) {
                cacheMatrix <<- invsereMatrix
        }
        
        getInverse <- function() cacheMatrix
        
        list(setMatrix = setMatrix, 
             getMatrix = getMatrix, 
             setInverse = setInverse, 
             getMatrix = getMatrix)
}


## cacheSolve
## This function uses solve() to get the inverse of a
## given matrix if it doesn't exist already.

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
