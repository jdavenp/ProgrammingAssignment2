## These functions create a matrix object that can cache its inverse


## makeCacheMatrix creates the matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) { #takes a matrix as an argument
        m <- NULL     # sets m to NULL 
        set <- function(y) { # resets so new matrix will get new inverse
                x <<- y
                m <<- NULL
        }
        get <- function() x # gets the matrix
        setinv <- function(inv) m <<- inv #sets the inverse to a value m
        getinv <- function() m #returns the inverse
        list(set = set, get = get, #creates a list of functions
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve either returns the cached value of the inverse matrix, if already
## created or it creates the inverse of a matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv() # see if there is a cached value by calling getinv()
        if(!is.null(m)) { # if m is not null we'll know that there is cached version
                message("getting cached data")
                return(m) # m will hold the value of getinv
        }
        data <- x$get() # otherwise, create a data variable with the matrix
        m <- solve(data, ...) # set m to the inverse of the matrix
        x$setinv(m) # store this value of the inverse so it will be cased for next time
        m # return the inverse
}
