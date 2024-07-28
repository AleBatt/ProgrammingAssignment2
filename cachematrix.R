## These two functions create a "special" object that stores a matrix
## and caches the inverse of the given matrix

## The makeCacheMatrix function creates a list containing functions to:
## (1)set:Set the value of the matrix
## (2)get:Get the value of the matrix
## (3)setInv:Set the value of the inverse of the matrix
## (4)getInv:Get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    set<-function(y){
        x<<-y
        inv<<-NULL
    }
    get<-function()x
    setInv <- function(solve) inv <<- solve
    getInv <- function() inv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}

## The cacheSolve function returns the inverse of the special "matrix" 
## created with the above function. However, it first checks to see if the
## inverse has already been calculated and cached. If so, it gets the inverse
## from the cache and skips the computation. Otherwise, it calculates the 
## inverse of the data and sets the value of the inverse in the cache via the 
## setInv function

cacheSolve <- function(x, ...) {
    inv <- x$getInv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInv(inv)
    inv
}
