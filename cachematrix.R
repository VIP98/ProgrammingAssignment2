##So this is my function. Honestly I don't really get the point of
## that task. I just wrote a function like that we can saw in the 
##example. It works same... So i hope that is good. :D
## (I just changed some words in the example.)

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL       ## from the example, inv = m
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(i) inv <<- i  ## from the example i=mean
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)    
}


cacheSolve <- function(x, ...) {
    inv <- x$getinv()  ## from the example too: "m <- x$getmean()"
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    matrix <- x$get() ## that is too, matrix = data
    inv <- solve(matrix, ...)
    x$setinv(inv)  ## from the example...
    inv
}
