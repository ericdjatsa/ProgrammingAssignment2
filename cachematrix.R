## The purpose of the two functions below is to provide an way to
## create a special object for storing a matrix, then subsequently retrieve the 
## inverse of the matrix making sure that it is computed only once then stored in
## a cache , so that further retrieval operation while not trigger the complex and costly 
## operations involved in computing a matrix's inverse

## The makeCacheMatrix function takes as input a matrix and returns a "special" vector containing
## a set of functions allowing to retrieve the matric, set the matrix, compute it's inverse or set it

makeCacheMatrix <- function(x = matrix()) {
        
        # Cache variable to store the matrix inverse, we initialize it to NULL
        inv <- NULL
        
        # Flag variable to tell if the matrix has changed or no
        isChanged <- FALSE
        
        #Function to set the value of matrix
        set <- function(y){ 
                # We assign the new value of x
                x <<- y 
                # Then we mark the matrix as changed
                isChanged <<- TRUE
        }
        
        # Function to get the matrix
        get <- function(){x}
        
        # Function to set the value of the matrix inverse
        setInv <- function(inverse) {inv <<- inverse}
        
        #function to get the inverse of the matrix
        getInv <- function(){inv}
        
        # Function to set if the matrix has changed or not
        setChanged <- function(c=FALSE){
                isChanged <<- c
        }
        
        # Function to get the status ( changed or not changed ) of the matrix
        getChanged <- function(){isChanged}
        
        # we finally return a list containing all set and get functions
        list(set = set, get = get, setInv = setInv, getInv = getInv,setChanged = setChanged, getChanged = getChanged)
        
}


## The cacheSolve function takes in input an object computed using the above 'makeCacheMatrix' function
## then first checks if the input matrix's inverse has already been computed and if the matrix hasnt been changed
## If so, it simply returns the cached value of the matrix's inverse, otherwise, it computes the inverse and store it
## in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        # Get the status of the matrix : changed or not changed 
        hasChanged <- x$getChanged()
        
        # get the cached value of the matrix inverse
        inv <- x$getInv()
        
        # If the cached value is not NULL and the matrix has not changed
        # then we simply return the cached value
        
        if( !is.null(inv) & !hasChanged ){
                message("Returning cached data")
                return (inv)
        }
        
        
        # If the cached value is NULL or the matrix has changed, then we need to (re)compute the inverse
        
        # retrieve the matrix
        data <- x$get()
        
        inv <- solve(data)
        
        # then we cache the new value
        x$setInv(inv)
        
        # and finally we set back the status flag to 'not changed' to avoid recomputation
        # at the next call of the cacheSolve function
        x$setChanged(FALSE)
        
        # we can now return the computed value
        return(inv)
        
}
