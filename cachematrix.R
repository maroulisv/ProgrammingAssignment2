## The makeCacheMatrix and cacheSolve functions are used to create a special 
## object that stores a matrix, and cache's its inverse matrix.

## The function makeCacheMatrix takes a matrix x as its argument (if the
## argument is not defined, then x is created as an empty matrix).This function
## returns a special "matrix" which it is a list containing the functions 
## set, get, setinv and getinv.

makeCacheMatrix <- function(x = matrix()) {          
        inv <- NULL                                  ## inv is the cached inverse matrix of the x matrix, initially it is set to NULL  
        set <- function(y) {                         ## The set function sets the value of the x matrix, and resets the value of inv to NULL
                x <<- y
                inv <<- NULL
        }
        get <- function() x                          ## The get function returns the value of x matrix
        setinv <- function(inverse) inv <<- inverse  ## The setinv function sets the cached value of the inverse matrix  
        getinv <- function() inv                     ## The getinv function returns the cached value of the inverse matrix
        list(set = set, get = get,                   ## create a list containing the set, get, setinv and getinv functions
             setinv = setinv,                        ## with names assigned to them
             getinv = getinv)
             
}

## The cacheSolve function takes the special "matrix" created by the makeCacheMatrix
## function as its argument and checks if it contains a cached value for the inverse matrix,
## and if it does, it returns the cached value, otherwise it uses the solve function to 
## calculate the inverse matrix, and stores it in the cache

cacheSolve <- function(x, ...) {                     
        inv <- x$getinv()                            ## assign the cached value of the inverse matrix in a local variable "inv"
        if(!is.null(inv)) {                          ## check if the cached value is not NULL, and if this is true, have the 
               message("getting cached data")        ## message "getting cached data" and the cached value printed
               return(inv)        
        }
        data <- x$get()                              ## If the cached value is NULL assign the value of the original matrix in a variable "data"
        inv <- solve(data, ...)                      ## use the solve function to calculate the inverse matrix of the original matrix
        x$setinv(inv)                                ## return the value that is calculated by the solve function, and set it as the cached value 
        inv
}
