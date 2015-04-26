# These functions can be used to create an object containing,
# the cached inverse of a matrix.

# This function is a factory that creates an object / list
# This object / list contains a variable to store the cached inverse.
# It also has get and set "property style" accessors,
# allowing access to the original matrix data and the cached inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


# This function takes as input the special object / list 
#   created by the function above.
# If a cached version of the inverse is available it is returned along with a message,
#   indicating that the result was obtained from the cache.
# If not it is calculated, stored in the cache and then returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}

## Unit test:
## load cachematrix.R using source then type the following in the consle
#> x <- matrix(c(4,2,7,6), nrow=2,ncol=2)
#> x
## The output should be:
#[,1] [,2]
#[1,]    4    7
#[2,]    2    6
#> cx <- makeCacheMatrix(x)
#> cacheSolve(cx)
## The output should be:
#[,1] [,2]
#[1,]  0.6 -0.7
#[2,] -0.2  0.4
