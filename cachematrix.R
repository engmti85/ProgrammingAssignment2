## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function creates a special "matrix" object that can cache its inverse
# makeCacheMatrix creates a list containing functions to
        # 1. set the value of the matrix
        # 2. get the value of the matrix
        # 3. set the value of inverse (solve) of the matrix
        # 4. get the value of inverse (solve) of the matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL  
        set <- function(y) {    
                x <<- y    
                m <<- NULL  
        }  
        get <- function() x  
        setsolve <- function(solve) m <<- solve  
        getsolve <- function() m  
        list(set = set, 
             get = get,       
             setsolve = setsolve,       
             getsolve = getsolve)
}


## Write a short comment describing this function
# The following function returns the inverse (solve) of the input matrix after first checking if the inverse has already been computed. 
# If it's cached it gets the result from the cach 
# If not it computes the inverse, sets the value in the cache and returns it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getsolve()
        if(!is.null(inv)) {
                message("getting inversed matrix data.")         
                return(inv)
        }
        data <- x$get()      
        inv <- solve(data) 
        x$setsolve(inv)      
        inv 
}
