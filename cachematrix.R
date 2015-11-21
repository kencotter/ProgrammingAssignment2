## Below is a pair of functions that cache the inverse of a matrix


## The first function, `makeCacheMatrix` creates a special "matrix", which is
## really a list containing a function to

## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse
## 4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## The following function calculates the inverse of the special "matrix"
## created with the above function

## First it checks to see if the inverse has already been calculated: if(!is.null(i))
## If so, it `get`s the inverse from the cache and skips the computation
## If it has not already been calculated then it calculates the inverse of
## the data and sets the value of the inverse in the cache via the `setinv`
## function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## x: output of makeCacheMatrix()
        
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
