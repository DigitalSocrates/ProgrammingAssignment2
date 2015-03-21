## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        ## set object's value ot null
        m <- NULL
        ## sets the matrix value 
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
        ## if inverse been calculated, get it and no work to be done
        m  <- x$getInverse()
        ## check if the object been cached before
        if (!is.null(m)){
                message("getting cached data")
                return(m)
        }
        ## get input matrix
        data  <- x$get()
        ## cache matrix
        x$set(data) 
        m  <- solve(data, ...)
        ## cache the inverse
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
