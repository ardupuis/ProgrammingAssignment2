## Use:
## a <- makeCacheMatrix(enter matrix here),
## then use cacheSolve(a) to invert the matrix from cache if present,
## live if not.

## The first function, makeVector creates a special "vector", 
## which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix


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


## The following function solves the special "vector" 
## created with the above function. However, it first checks to see if 
## the inverted matrix is already present in cache. If so, it gets the 
## inverted matrix from the cache and skips the solve function. Otherwise, 
## it inverts the matrix and sets the value of the matrix in the cache via 
## the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting inverted matrix")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
