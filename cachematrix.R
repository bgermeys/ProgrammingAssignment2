## Matrix inversion is usually a costly computation and their may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly. 

## This pair of functions calculate the inverse of a matrix, store it in cache, 
##and return the cached result when called again. 

## This functions assume that the matrix supplied is always invertible.

## The first function creates a special "matrix" object that can cache its inverse.
## It is a list containing a function to
##	1.	set the value of the matrix
##	2.	get the value of the marix
##	3.	set the value of the inverse
##	4.	get the value of the inverse
	
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {  
                x <<- y
                m <<- NULL
        }                  
        get <- function() x 
        setinverse <- function(inverse) m <<- inverse 
        getinverse <- function() m 
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This second function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) { 
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

##example of use
##a<-makeCacheMatrix(matrix(1:4,2,2))   # creates a simple 2x2 matrix
##cacheSolve(a)  ## sets the inverse 
##cacheSolve(a)  ## second time, gets the cached inverse 
