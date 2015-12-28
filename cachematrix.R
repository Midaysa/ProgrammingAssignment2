#Function that creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(matrix = numeric()) {
    inv <- NULL                    #inv = valor de inv cacheada
    set <- function(m) {
        matrix <<- m
        inv <<- NULL
    }
    
    get <- function() matrix
    set_inverse <- function(inverse) inv <<- inverse
    get_inverse <- function() inv
    list(set = set, get = get, set_inverse = set_inverse, 
         get_inverse = get_inverse)
}

#Computes the inverse of the special "matrix" returned by makeCacheMatrix.
#If the inverse has already been calculated (and the matrix has not changed),
#then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(matrix, ...) {
    inv <- matrix$get_inverse()
    
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    data <- matrix$get()
    inv <- solve(data, ...)
    matrix$set_inverse(inv)
    inv
}