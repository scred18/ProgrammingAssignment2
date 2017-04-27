## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#Make special list of four functions working on matrix
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {    #set the matrix
                x <<- y
                inverse <<- NULL
        }
        get <- function() x     #get the matrix data
        setinverse <- function(i) inverse <<- i         #set the inverse matrix value in cache
        getinverse <- function() inverse                #get the cached matrix value
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()               #try to get the cached inverse matrix
        if(!is.null(inverse)) {                 #if cached inverse matrix exists
                message("getting cached data")
                return(inverse)                 #return the cached value
        }
        data <- x$get()                         #if cache doesn't exist, try solve the inverse from raw data
        inverse <- solve(data, ...)             #calculate the inverse matrix
        x$setinverse(inverse)                   #save the inverse matrix to cache for further use
        inverse                                 #return the calculated or cached inverse matrix
}
