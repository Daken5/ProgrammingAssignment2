## Creates a special "Matrix" object that can take 4 commands
## set, get, setsolve, getsolve
makeCacheMatrix <- function(m = matrix()) {
    ## initialize inverse matrix n as NULL
    n <- NULL
    
    ## define "set" function to 
    ## change existing "m" matrix 
    ## and reset the "n" matrix
    set <- function(y){
        m <<- y
        n <<- NULL
    }
    
    ## define "get" function to 
    ## return the existing "m" matrix
    get <- function() m
    
    ## define "setsolve" function to 
    ## set a new "n" matrix
    setsolve <- function(inverse) n <<- inverse
    
    ## define "getsolve" function to 
    ## get the existing "n" matrix
    getsolve <- function() n
    
    ##return a list of functions
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## Retrieves the inverse of a matrix from an object if it exists 
## otherwise it will compute the inverse of the matrix and update the object
cacheSolve <- function(x, ...) {
    
    ## check if inverse matrix "n" already exists and if so loads from cache and returns "n"
    n <- x$getsolve()
    if(!is.null(n)){
        message("getting cached data")
        return(n)
    }
    
    ## otherwise gets matrix data, calculates inverse "n", 
    ## then updates "n" and returns "n"
    data <- x$get()
    n <- solve(data)
    x$setsolve(n)
    n
}
