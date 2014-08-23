#creates a matrix and 4 methods to set, get, set inverse, get inverse of this matrix.
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        
        #set inverse variable in parent environment
        setInverse  <- function(inverse) i <<- inverse
        getInverse <- function() i
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

# calculates the inverse of the matrix
cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        
        # if the inverse already exists, returns the previous calculated value
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        # if the inverse doesn't exists, get the matrix, calcule it's inverse and set the result as a cached value
        data <- x$get()
        i <- solve(data)
        x$setInverse(i)
        i
}