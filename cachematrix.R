## Cache Inverse of the Matrix
## Run Using:
## x = rbind(c(1, 2), c(3, 4))
## m = makeCacheMatrix(x)
## cacheSolve(m) - First Run where the data returned is non-cached
## cacheSolve(m) - Second Run where the data returned is cached

## makeCacheMatrix Function - Create a matrix that can cache its inverse
## Args - x (A Matrix)
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Computation of Matrix inverse. If computation is already done, cached inverse is returned.
## Args - x (A Matrix), ... (Extra Arguments)
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
        ## Return a matrix that is the inverse of 'x'

		}


