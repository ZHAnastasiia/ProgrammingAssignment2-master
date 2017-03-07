## makeCacheMatrix defines a function that can cache its inverse. It defines
##setinverse() function that sets the data value within the object, and
##getinverse() function that retrieves the inverse value of the matrix from
##makeCacheMatrix() environment

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


## cacheSolve works together with makeCacheMatrix to populate or retrieve an
## inverse from the matrix of type makeCacheMatrix()
         # if the value exists already, it retrieves the inverse, if not - it calculates one and caches it in.

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
}


