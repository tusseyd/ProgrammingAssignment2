## These two functions, "makeVector" & "cacheSolve", work together to create and retrieve the inverse of a matrix.

## The first function establishes a list of setter and getter functions associated with the matrix 'x' 
## which the second function uses.

## The function 'makeVector' returns a list which contains four functions.
## These fours functions provide the ability to set/get the matrix, and set/get the inverse.

## The function 'cacheSolve' returns the inverse of the matrix 'x'.
## If the inverse has already been computed, it is retrieved from cache.
## If the inverse has not already been computed, it will compute the inverse store it.

###############################################################################################################

makeVector <- function( x = matrix() ) {
        ## This function takes a matrix as an argument and returns a list of functions for that matrix.
	
        ## Initially the inverse (stored in the variable "inv") is set to NULL as no value has been computed.
        inv <- NULL
                
                ## The 'set' function stores the matrix 'y' in variable 'x'.
                ## Note that 'x' is in a different environment than the 'set' function,
		## requiring use of the <<- assignment operator.	
                ## After matrix 'y' is stored in the variable "x",
		## the variable 'inv' is set to NULL, removing any previous value (i.e. the cache is cleared).
                set <- function( y ) {
                        x <<- y
                        inv <<- NULL
                }
                
                ## The 'get' function returns the value of the matrix "x" (stored by the "set" function).
                get <- function() x
                
                ## The 'setinv' function assigns the value of the inverse to the variable 'inv'.
                setinv <- function( inverse ) inv <<- inverse
                
                ## The 'getinv' function returns the value of the inverse previously stored in variable "inv".
                getinv <- function() inv
                
                ## Finally, the function 'makeVector' returns this list of functions.
                list( set = set, get = get, setinv = setinv, getinv = getinv )
        }

###############################################################################################################

## The 'cacheSolve' function returns the inverse of a matrix 'x'. 
## It first checks to see if the inverse has previously been calculated (indicated by a non-NULL value of the variable "inv").
## If so, it retrieves the inverse from cache using the getinv' function on the matrix 'x. 

## However, if 'getinv' returns a NULL value (indicating no inverse is available in cache), 
## then the matrix 'x' is retrieved, the inverse is computed using the R 'solve' function,
## and then stored for future use using the 'setinv' function.

cacheSolve <- function( x, ... ) {
## Returns the inverse of matrix 'x'.
## If the inverse is not in cache, it is computed and stored for future use.
        
        ## Retrieve the inverse for matrix 'x'
        inv <- x$getinv()
	
        ## Test to see if the inverse is non-NULL. If not NULL, retrieve the value of the inverse from cache.
	## Then exit the function via the return() function.
        if( !is.null( inv ) ) {
                message( "Retrieving the inverse from cached data." )
                return( inv )
        }
        
        ## If the "inv" value is NULL, it indicates that no inverse has been computed. 
	## In that case, this section retrieves the matrix 'x', computes the inverse using the R 'solve' function,
	## and then stores the inverse using the setinv' function so that it is available for future use.
	## The value of the inverse is then returned.
        data <- x$get()
        inv <- solve( data, ... )
        x$setinv( inv )
        inv
}
