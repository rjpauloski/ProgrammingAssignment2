## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        
        ## 1. set the value of the matrix
        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## 2. get the value of the matrix

        get <- function() x
        
        ## 3. set the value of the inverse
        
        setmatrix <- function(mean) m <<- mean
        
        ## 4. get the value of the inverse
        
        getmatrix <- function() m
        
        ## 5. turn into list
        
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
        
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
