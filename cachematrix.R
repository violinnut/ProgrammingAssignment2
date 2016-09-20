## The overall purpose of these two functions is to store a 
## cached matrix inverse and retrieve the cached value when appropriate.

## makeCacheMatrix takes our initial matrix as an input and stores the value
## in the global environment.

makeCacheMatrix <- function(x = matrix()) {
    inv<- NULL
    set<-function(y){
        x <<- y
        inv <<-NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve checks to see if a cached matrix inverse is stored and then 
## returns that value if it exists. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
