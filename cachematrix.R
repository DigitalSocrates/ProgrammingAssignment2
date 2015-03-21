## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list(
                set = set, 
                get = get,
                setInverse = setInverse,
                getInverse = getInverse
                )
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        m  <- x$getInverse()
        if (!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data  <- x$get()
        m  <- solve(data, ...)
        #set 
        x$setInverse(m)
        #return the inverse
        m
}

## to run / test do the following
## > tempMatrix <- matrix (1:4, 2, 2)
## > cachedMatrix <- makeCacheMatrix(tempMatrix)
## > cacheSolve(cachedMatrix)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
