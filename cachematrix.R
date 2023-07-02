## OBS: if I understand the purpose of this assignment correctly, it is a way to understand how closure works in computing


## this function will create a special type of
## matrix whose capabilities include the caching ability

makeCacheMatrix <- function(x = matrix()) {
        
        ## at first, we don't have any cached result because of course 
        ## we haven't made any calculation
        ## so inv is set to NULL
        inv <- NULL
        
        ## modifies the matrix and set it to a new value
        set <- function(y)
        {
                x <<- y
                inv <<- NULL
        }
        ## returns the current matrix
        get <- function() x
        ## here we will cache the result
        set_inverse <- function(inverse) inv <<- inverse
        ## return the result
        get_inverse <- function() inv
        list(
                set = set,
                get = get,
                set_inverse = set_inverse,
                get_inverse = get_inverse
        )
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        ##  it will look if the inverse of x already exists and if it does, it will return 
        ## the cached result
        inv <- x$getinverse()
        if(!is.null(inv))
        {
                message("getting cached data")
                return (inv)
        }
        ## if the inverse hasn't been already cached
        ## we will get the value of x and concatenate it with the arguments passed on to the function
        ## and the function will try to calculate the inverse of the matrix
        data <- x$get()
        inv <- solve(data, ...)
        ## here will cache the result
        x$set_inverse(inv)
        ## finally we return the result
        inv
}
