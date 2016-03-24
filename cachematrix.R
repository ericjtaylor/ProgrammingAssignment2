## Functions to cache the inverse of a matrix


## Creates an enclosure that stores the parameter matrix 'x' and its cached inverse
## There are functions allowing you to interface with the enclosure (get/set the matrix, get/set the inverse)

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(calc) inv <<- calc
    getinv <- function() inv
    list(set = set,
         get = get,
         setinv = setinv,
         getinv = getinv)
}


## Interfaces with a makeCacheMatrix object 'x' and returns the inverse to the matrix stored within it
## If the inverse is not cached, it will calculate and cache it

cacheSolve <- function(x) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}
