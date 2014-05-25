##  makeCacheMatrix is created whos inverse 
##value needs to be cahced

makeCacheMatrix <- function(x = matrix()) {
    # inverse is null when the function is loaded initially
    inv <- NULL
    
    # set the matrix function
    set <- function(y)  {
    # << is used since x and i or not the function that's currently being called
    x <<- y
    inv <<- NULL
      
    }
  
    ## get functions gets the matrix
    get <- function() x
    ## setinv sets the value of inv
    setinv <- function(solve) inv <<- solve 
    ## getinv gets the value of the inv
    getinv <- function() inv
    list(set=set, get = get , setinv = setinv, getinv = getinv)

}



##cahseSolve Computes, caches, and returns matrix inverse

cacheSolve <- function(x, ...) {
    
    ##return the inverse value
    inv <- x$getinv()
    
    ## check if the inv is cached
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
        }
    
    ## calculate inv and cache inverse
    data <- x$get()
    inv <- solve(data,...)
    x$setinv(inv)
    inv
}
