## this function has two parts, which are explained as follows: 
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


## The first function, makeVector creates a special "vector", which is really a list containing a function to
## set: set the value of the matrix
## get: get the value of the matrix
## setinverse:  set the inverse of the  matrix
## getinverse: get the inverse of the matrix 


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The following function calculates the inverse of the special "vector" created with the above function. 
## However, it first checks to see if the inverse of the matrix has already been calculated. If so, it gets the inverse value from the cache 
## and skips the computation. Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache 
## via the setinverse function.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
