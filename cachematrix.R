## makeCacheMatrix creates a "matrix" object that can cache its inverse.
## cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above

## Function to create a matrix and cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse<-NULL
        #cache the value of the matrix
        
        set<-function(y){
                x<<-y
                inverse<<-NULL
        }
        get<-function() x #get the value
        setinverse<-function(inverse_temp) inverse<<-inverse_temp #set the inverse
        getinverse<-function() inverse #get the inverse of the matrix
        #list containing the elements of the matrix
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)

}


## Function to compute the inverse of matrix returned by makrCacheMatrix

cacheSolve <- function(x, ...) {
        #get the inverse of the matrix from cache
        i<-x$getinverse()
        #check if the inverse of the matrix exists in the cache. If yes then return the value from cache
        if(!is.null(i)){
                message("Getting cached data")
                return(i)
        }
        #create the variable data and assign the matrix value to it
        data<-x$get()
        #calculate the inverse of the matrix
        i<-solve(data)
        #set the value of the inverse of the matrix in the cache
        x$setinverse(i)
        #return the inverse of the matrix
        i
}
