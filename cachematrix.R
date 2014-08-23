## The purpose of the two functions below is to provide a way to create a special 
## object for storing a matrix, then subsequently retrieve the inverse of 
## the matrix making sure that it is computed only once then stored in a cache
## so that further retrieval operations while not trigger the complex and costly
## operations involved in computing a matrix's inverse

## The makeCacheMatrix function takes as input a matrix and returns a "special" 
## vector containing a set of functions allowing to retrieve the matric, 
## set the matrix, compute it's inverse or set it

makeCacheMatrix <- function(x = matrix()) {
        
        # Cache variable to store the matrix inverse, we initialize it to NULL
        inv <- NULL
        
        # Flag variable to tell if we need to recompute or not the matrix inverse
        # (for example because the matrix has changed )
        # we set the flag's initial value to TRUE 
        # because at object creation the matrix can be considered as changed
        isChanged_recompInv <- TRUE
        
        #Function to set the value of matrix
        set <- function(y){ 
                # We assign the new value of x
                x <<- y 
                # Then we mark the matrix as changed, and indicate that the inverse 
                # has to be recomputed
                isChanged_recompInv <<- TRUE
        }
        
        # Function to get the matrix
        get <- function(){x}
        
        # Function to set the value of the matrix inverse
        setInv <- function(inverse) {inv <<- inverse}
        
        #function to get the inverse of the matrix
        getInv <- function(){inv}
        
        # Function to indicate if we need to recompute the inverse 
        # (in case the matrix has changed) or not 
        setRecompInv <- function(flag=TRUE){
                isChanged_recompInv <<- flag
        }
        
        # Function to get the flag wich says if we need to recompute or not 
        # the matrix's inverse
        getRecompInv <- function(){isChanged_recompInv}
        
        # we finally return a list containing all set and get functions
        list(set = set, get = get, setInv = setInv, getInv = getInv,
             setRecompInv = setRecompInv, getRecompInv = getRecompInv)
        
}


## The cacheSolve function takes in input an object computed using the above 
## 'makeCacheMatrix' function  then first checks if the input matrix's inverse 
## has already been computed and if the matrix hasnt been changed
## If so, it simply returns the cached value of the matrix's inverse, otherwise,
## it computes the inverse and store it in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        # Get the status of the matrix inverse  : to be recomputed or not ? 
        recompInv <- x$getRecompInv()
        
        # get the cached value of the matrix inverse
        inv <- x$getInv()
        
        # If the cached value is not NULL and the status flag says we don't
        # need to recompute the inverse (matrix has not changed)
        # then we simply return the cached value
        
        if( !is.null(inv) & !recompInv ){
                message("Returning cached data")
                return (inv)
        }
        
        
        # If the cached value is NULL or the matrix has changed, then we need to
        # (re)compute the inverse
        
        # retrieve the matrix
        data <- x$get()
        
        inv <- solve(data)
        
        # then we cache the new value
        x$setInv(inv)
        
        # and finally we set back the status flag for recomputing the inverse to 
        #'FALSE' to avoid recomputation at the next call of the cacheSolve function
        x$setRecompInv(FALSE)
        
        # we can now return the computed value
        return(inv)
        
}
