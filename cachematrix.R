## makeCacheMatrix creates a "matrix" object that can cache its inverse.
## cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above

## Function to create a matrix and cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse<-NULL
        set<-function(y){
                x<<-y
                inverse<<-NULL
        }
        get<-function() x
        setinverse<-function(inverse_temp) inverse<<-inverse_temp
        getinverse<-function() inverse
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)

}


## Function to compute the inverse of matrix returned by makrCacheMatrix

cacheSolve <- function(x, ...) {
        #l<-makeCacheMatrix(x)
        i<-x$getinverse()
        if(!is.null(i)){
                message("Getting cached data")
                return(i)
        }
        data<-x$get()
        i<-solve(data)
        x$setinverse(i)
        i
}
