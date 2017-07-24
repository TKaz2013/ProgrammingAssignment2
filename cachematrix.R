## Programming Assignment 2
## makeCacheMatrix is a function designated to create a special square matrix for inverse matrix calculations
## the makeCacheMatrix will prepare a a set functions to pass to cacheSolve.
## cacheSolve will relay these functions and cache designated inverse calculations
##
## cacheSolve requires the proterties assigned by makeCacheMatrix to proceed
## try defining x <- rbind(c(1,-.25) , c(-0.25,1))
## P <- makeCacheMatrix (x)
## cacheSolve(P)
## ________________________________________________________________________________________________

makeCacheMatrix <- function(x = matrix()) {
        
        ## inputs to makeCacheMatrix will retain properties of the mackeCache environment
        ## INV will be used as a value of the inverse matrix of the squre matrix x
        
        
        INV <- NULL                             ## x and INV are initialized by the code for later use
        set <- function(y) {    
                x <<- y                         ## Set assigns a value to x
                INV <<- NULL                    ## set assigns NULL to the matrix INV
        }                                       ##clearing previously stored cacheSolve data
        get <- function() x                     ## X is retrieved from the parent function makeCacheMatrix
        setinv <- function(solve) INV <<- solve ## setinv sets the inverse of x to the previously defined INV
        getinv <- function() INV                ## getinv retrieves the newly set value of INV
        
        
        ## 'set', 'get', 'setinv', and 'getinv' relays these values to their respective above functions
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
        

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        INV <- x$getinv()                       ## This calls for the inverse of the orginal input x
        
        if(!is.null(INV)) {                     ## Here cacheSolve checks for an existing inverse of x
                message("getting cached data")  ## if a cached inverse exists the messeage is delivered
                return(INV)                     ## the Inverse matrix is returned
        }
        
        
        data <- x$get()                         ## if the inverse is not cached, input x is given to data
        INV <- solve(data, ...)                 ## the inverse of data is set to the matrix INV
        x$setinv(INV)                           ## the x inverse is retrieved after being cached
        INV                                     ## the inverse matrix is returned to the console
        
        
        
        
        
}
