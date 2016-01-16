# This function stores a matrix and caches its inverse

makeCacheMatrix <- function(mat = matrix()) {
    # reset the matrix and inverse
    inv <- NULL

    # set the matrix into a given matrix
    # lazy evaluate inverse, so just reset it, but do not compute it yet 
    set <- function(y) {
        mat <<- y
        inv <<- NULL
    }

    get <- function() mat

    # get and set methods for inverse
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv

    ## return a list of getter and setter methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


# This function returns inverse of the cacheMatrix object passed into it
# If the inverse has not been computed, it computes and caches it first

cacheSolve <- function(m, ...) {
    inv <- m$getInverse()

    # If the inverse is not calculated yet, then calculate it
    if(is.null(inv)) 
    {
       data <- m$get()
       inv <- solve(data, ...)
       m$setInverse(inv)
    }

    return(inv)
}


test <- function() 
{
    cat("Testing makeCacheMatrix function\n")

    # create a matrix and take its inverse directly
    matA <- matrix(c(0,1,2,1,0,1,2,0,1), nrow=3, ncol=3)
    invA <- solve(matA)
    cat("\nMatrix A:\n")
    print(matA)

    cat("\n... its inverse:\n")
    print(invA)

    # Now do the same through our function:
    matrixObj <- makeCacheMatrix(matA)
    cat("\n... and its cached inverse:\n")
    invA_cached <- cacheSolve(matrixObj)   # Computes, caches, and returns the matrix inverse
    print(invA_cached)

    cat("\nNow we repeat to check if the cached value is cleared after an uptade\n")

    # create a matrix and take its inverse directly
    matA <- matrix(c(2,1,0,1,0,1,0,1,2), nrow=3, ncol=3)
    invA <- solve(matA)
    cat("\nMatrix A:\n")
    print(matA)

    cat("\n... its inverse:\n")
    print(invA)

    # Now do the same through our function:
    matrixObj <- makeCacheMatrix(matA)
    cat("\n... and its cached inverse:\n")
    invA_cached <- cacheSolve(matrixObj)   # Computes, caches, and returns the matrix inverse
    print(invA_cached)

    cat("\nTest done\n")
}   

test()
