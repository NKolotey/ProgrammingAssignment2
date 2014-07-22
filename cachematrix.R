# Example usage:
# > x <- matrix(rnorm(16), nrow = 4)          // Create a matrix x
# > cx <- makeCacheMatrix(x)                  // Create our special matrix
# > cx$get()                                  // Return the matrix
# > cacheSolve(cx)                            // Return the inverse
# > i <- cacheSolve(cx)                       // Call the 2nd time, so return
# > round(x %*% i, digits = 9)                // Ensure (x %*% i) is identity matrix
# 
# makeCacheMatrix: return a list of functions to:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        # inv will store the cached inverse matrix
        inv <- NULL
        
        # Setter for the matrix
        set <- function(x) {
                x <<- y
                inv <<- NULL
        }
        
        # Getter for the matrix
        get <- function() x
        
        # Setter for the inverse
        setSolve <- function(solve) inv <<- solve
        
        # Getter for the inverse
        getSolve <- function() inv
        
        # Return the matrix with our newly defined functions
        list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}



# cacheSolve: Compute the inverse of the matrix. If the inverse is already
# calculated before, it returns the cached inverse.
cacheSolve <- function(x, ...) {
        ## Get the current cached value of the inverse of 'x'
        inv <- x$getSolve()

        # If the inverse is already calculated, return it
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        # The inverse is not yet calculated, so we calculate it
        data <- x$get()
        inv <- solve(data, ...)
        
        # Cache the inverse
        x$setSolve(inv)
        
        # Return it
        inv
}
