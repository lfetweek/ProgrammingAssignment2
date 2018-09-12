## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##Similar to the function given in the example for the 2nd assignment (makeVector function)
##makeCacheMatrix creates a special "matrix", which is a list containing a function to the following:

##1 set the value of the matrix
##2 get the value of the matrix
##3 set the value of the matrix (inverse)
##4 get the value of the matrix (inverse)


makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    set <- function(y){
        x<<-y
        inv<<-NULL
    }
    
    get<-function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
    
    
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv<- x$getinv()
    if(!is.null(inv)){
        message("getting cached data!")
        return(inv)
    }
    data<-x$get()
    inv<-solve(data)
    x$setinv(inv)
    inv
    
}
