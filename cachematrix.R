## These two functions work togetther to allow for the creation
## and retrieval of the inverse of a matrix.

## The first function establishes a list of "setters" and "getters" associated
## with the matrix 'x' which the second function uses.

## The function 'makeCacheMatrix' returns a list which contains four functions.
## These fours functions provide methods (to use O-O) to set/get the matrix, and
## to setinv and getinv. Use of "getters" and "setters" is a standard 
## approach to object-oriented programming. This is how they are implemented
## in R.

## The second function 'cacheSolve' returns the inverse of the matrix 'x'.
## If the inverse has already been computed, then it is retreived with the 
## 'getinv' function.  If the matrix inverse has not already been computed, 
## then the 'getinv' function will return a NULL value, and the remainderof
## the function will be executed to generate the inverse
## (with the 'solve' function), and then store that inverse
## (with the 'setinv' function)


makeCacheMatrix <- function(x = matrix()) {
        ## This function takes a matrix as an argument.
        ## Initiially the inverse ("inv") is set to NULL
        
        inv <- NULL
                
                ## The 'set' function stores the matrix 'y' in varialbe 'x'
                ## Note that 'x' is in a different environment than the 'set'
                ## function, and hence the use of the <<- assignment operator.
                ## After the matix 'x' is stored, the variable 'inv' is
                ## set to NULL, which removes any previous matrix inverse.
                ## This means that every time a new matrix is 'set', the
                ## inverse is removed, even if the matrix is the same. This
                ## prevents any misuse of the inverted matrix after a the 
                ## 'set' function is called. Essentially the cache is cleared.
                set <- function(y) {
                        x <<- y
                        inv <<- NULL
                }
                
                ## The 'get' function returns the value of the matix.
                ## It should be noted that the 'set' function must be called
                ## before any use of the 'get' function; otherwise
                ## an error will result.
                get <- function() x
                
                ## The 'setinv' function stores the value of the inverse
                ## into the variable 'inv'.
                setinv <- function(inverse) inv <<- inverse
                
                ## The 'getinv' function returns the value of the inverse that was
                ## set with the 'setinv' function
                getinv <- function() inv
                
                ## The call to the 'makeCacheMatrix' returns this list, which is
                ## a list of functions inside of 'makeCacheMatrix'
                list(set = set, get = get,
                     setinv = setinv,
                     getinv = getinv)
        }
}


## The 'cacheSolve' function can be called to return the inverse of a matix 'x'
## When called, the function first retrieves the value of the inverse using the
## 'getinv' function on the matix 'x. If the inverse has been calculated,
## a non-NULL value is returned and then the cached inverse in the variable 'inv'
## is returned.

## However, if 'getinv' returns a NULL value, then the matrix 'x' is retrieved
## and the inverse is computed using the R 'solve' method. This inverse is then
## stored for future use using the 'setinv' function.

cacheSolve <- function(x, ...) {
## Returns a matrix that is the inverse of 'x'
        
        ## 'get' the inverse for matrix 'x'
        inv <- x$getinv()
        
        ## Test to see if the inverse is non-NULL, if TRUE, then return
        ## the retrieved value 'inv' which is cached. Then exit the function
        ## via the return command.
        if(!is.null(inv)) {
                message("getting cached data for inverse")
                return(inv)
        }
        
        ## If the inverse is NULL, then this branch will be executed.
        ## This part of the function gets the vector 'x' and computes the inverse
        ## using the R 'solve' function. It then stores the inverse using the
        ## 'setinv' function so that it is available for future use.
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
