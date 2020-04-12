makeCacheMatrix <- function(x = matrix()) {
        
        #Reset the value of inverse matrix when creating a new matrix
        inverseM <- NULL
        
        #A function to return the matrix to calculate the inverse later on
        get <- function() x
        
        #A function to store the results of the matrix inversion in the global 
        #variable
        setInverse <- function(inv) {
                inverseM <<- inv
        }
        
        #A function to get the cached results of the matrix inversion stored in
        #the global variable
        getInverse <- function() inverseM
        
        #Returning the list of functions as a result
        list(get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
}

#A custom "solve" function to catch an error if the matrix is not square

mySolve <- function(x) {
        
        #By default assume that the matrix is not invertable and assing 
        #the error string as the result of inversion
        
        inv <- "Can't invert this matrix"
        
        #Try to update the variable with the results of the inversion
        try(
                {
                        inv <- solve(x)
                }
        )
        
        #Return the results, either updated or not
        inv
}


cacheSolve <- function(x, ...) {
        
        #Get cached results of the calculation if the matrix inversion
        inv <- x$getInverse()

        #Check if the stored value is not empty
        if (!is.null(inv)) {
                
                #Returning cached results
                message("Getting cached data")
                return(inv)
                
        }
        
        #If the cached data was empty this means it was not calculated before
        #and needs to be calculated now. Get the matrix first.
        m <- x$get()
        
        #Use the custom solve function to handle to handle the error during 
        #inversion if the matrix is not square
        inv <- mySolve(m)

        #Update the global variable with the inversion results
        x$setInverse(inv)
        
        #Return the result of inversion
        return(inv)
        
}
