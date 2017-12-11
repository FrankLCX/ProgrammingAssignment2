
## This is the submission of Programming Assignment 2.  
## The following funtions aims to cache the inverse of 
## a matrix without the repeating long calculations. 

## FUNCTION 1:
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x<<-y
                i<<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set=set, get=get, 
             setinverse=setinverse, 
             getinverse=getinverse)
}


## FUNCTION 2: 
## It firstly tests whether the inverse is already cached, if not
## it then computes the inverse of the matrix

cacheSolve <- function(x, ...) {
         i <- x$getinverse()
        if(!is.null(i)){
                message("getting cached inverse")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i       ## Return a matrix that is the inverse of 'x'
}
     
## NAME: FRANK LIU
