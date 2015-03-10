## The pair of functions below cache the inverse of a matrix
## 

## makeCacheMatrix function creates a special "matrix" object that can cache
## its inverse.


makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        set_inverse <- function(inverse) i <<- inverse
        get_inverse <- function() i
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}


## cacheSolve function calculates the inverse of the special "matrix" created
## with makeCacheMatrix function above. It first checks to see if the inverse
## has already been calculated. If so, it gets the inverse from the cache and
## skips the computation. Otherwise, it calculates the inverse of the matrix
## and sets the value of the inverse in the cache.

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$get_inverse()
        if (!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$set_inverse(i)
        i
}
