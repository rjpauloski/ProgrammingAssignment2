## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        
        ## 1. set the value of the matrix function
        
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        ## 2. get the value of the matrix function

        get <- function() x
        
        ## 3. set the value of the inverse function
        
        setinvm <- function(inverse) inv <<- inverse
        
        ## 4. get the value of the inverse function
        
        getinvm <- function() inv
        
        ## 5. make matrix into list to be able to process further with cacheSolve
        
        list(set = set, get = get, setinvm = setinvm, getinvm = getinvm)
        
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m        
        
}
