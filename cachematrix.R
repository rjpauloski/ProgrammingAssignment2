## Below are a pair of functions that create and cache the inverse of a matrix.
## Caching the inverse of a matrix rather than computing it repeatedly saves computuation cycles.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## makeCacheMatrix sets/gets the value of the matrix and hten the inverse and then
## puts into list form for evaluation.

makeCacheMatrix <- function(x = matrix()) {
        ## Create special matrix object
        
        ## 1. set the value of the matrix
        
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        ## 2. get the value of the matrix

        get <- function() x
        
        ## 3. set the value of the inverse
        
        setinvm <- function(inverse) inv <<- inverse
        
        ## 4. get the value of the inverse
        
        getinvm <- function() inv
        
        ## 5. coerce matrix into list
        
        list(set = set, get = get, setinvm = setinvm, getinvm = getinvm)
        
}

## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cacheSolve retreives the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of one created
        
        ## 1. get inverse of original matrix (may not be calucated, yet, check to follow)
        
        inv <- x$getinvm()
        
        ## 2. checks to see if inverse has been calculated
        
        if(!is.null(inv)) {
                message("getting cached data") ## 2.a. informs the cache exists
                return(inv)  ## 2.b. uses cached data vs recalculate
        }
        
        ## 3. if no cache data, calculates the inverse
        
        data <- x$get()
        inv <- solve(data, ...)
        
        ## 4. set value of the inverse matrix into the cache
        
        x$setinvm(inv)
        
        ## 5. output the inverse matrix
        
        inv
        
}