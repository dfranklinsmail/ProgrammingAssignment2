## cachematrix is helper function wrapping the logic to inverse a matrix so
## that results can be cachable. These functions are useful when the same matrix
## will be inversed many.

## The inverse of the matrix will only ever be calculated once. The next time it
## is required the priviously calculated value will be returned

## makeCacheMatrix is a helper function that contains getters and setters for a
## matrix and its inversion.
makeCacheMatrix <- function(x = matrix()) {
    inversion <- NULL
    set <- function(y) {
        x <<- y
        inversion <<- NULL
    }
    get <- function() x
    setinversion <- function(matrixInversion) inversion <<- matrixInversion
    getinversion <- function() inversion
    list(set = set, get = get,
         setinversion = setinversion,
         getinversion = getinversion)
}


## cacheSolve function will return the inversion of the passed in matrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inversion <- x$getinversion()
    if(!is.null(inversion)) {
        message("getting cached data")
        return(inversion)
    }
    matrix <- x$get()
    inversion <- solve(matrix)
    x$setinversion(inversion)
    inversion
}
