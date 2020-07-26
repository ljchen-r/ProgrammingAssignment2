## Below are two functions that cache the inverse of a matrix

## The first function: makeCacheMatrix creates a special matrix object 
## that can cache its inverse
## Set the values of an invertible matrix 
## Get the values of the invertible matrix 

makeCacheMatrix <- function(x = matrix()) {
            In <- NULL
            set <- function(y) {
                    x <<-y
                    In <<- NULL
            }
            get <- function() x
            list(set = set, get = get)
}


## The second function: cacheSolve computes the inverse of the special 
## matrix returned by the first function makeCacheMatrix. If the inverse
## has already been calculated and the matrix has not changed, then 
## cacheSolve retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        In <- x$set()
        if(!is.null(In)) {
              message("getting cached data")
              return(In)
        }
        data <- x$get()
        In <- solve(data)
        x$set(In)
        In
}
