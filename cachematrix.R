## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        l <- NULL
        set <- function(y) {
                x <<- y
                l <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) l <<- inverse
        getinverse <- function() l
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Write a short comment describing this function: This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. If the inverse has already been calculated (and the matrix has not changed), then `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        l <- x$getinverse()
        if(!is.null(l)){
                message("getting cached data")
                return(l)
        }
        data <- x$get()
        l <- solve(data, ...)
        x$setinverse(l)
        l
}

