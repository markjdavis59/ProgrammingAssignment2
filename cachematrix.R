# this code creates two functions
# makeCacheMatrix which calculates the inverse of a matrix and caches it
# and cacheSolve which returns the cached inverse if the matrix is unchanged
# otherwise calculates the inverse from scratch


makeCacheMatrix <- function(x = matrix()) {
# creates a function makeCacheMatrix
# which takes as its argument an object x which is a matrix
# and performs a series of operations

        inv <- NULL
# creates an object inv (which is going to be the inverse of the matrix x)
# and sets it initially as NULL

        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
# creates a function set taking an argument y
# this function sets x as the object y using the superassignment operator
# which means R will not search for x in local environment of the set function
# but in the parent environment(the makeCachematrix function's environment)
# set also resets inv as NULL, again searching for inv in the parent environment

        get <- function() x

# creates a function get
# which returns the matrix x

        setinverse <- function(solve) inv <<- solve

# creates a function setinverse
# which uses solve to calculate the inverse of the matrix
# and assigns the inverse to inv (in the parent environment)

        getinverse <- function() inv

# creates a function getinverse which returns the value of inv

        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
# creates a list containing the four functions by makeCacheMatrix
}



cacheSolve <- function(x, ...) {
# creates a function cacheSolve
# which takes a matrix as its argument

        inv <- x$getinverse()
# uses getinverse to return the inverse of the matrix passed to cacheSolve
 
        if(!is.null(inv)) {
                message("getting inverted matrix")
                return(inv)
# assesses whether inv is NULL
# if inv is not NULL, prints the message and returns the inverse of the matrix

        }
        data <- x$get()
        inv <- solve(data, ...)
# if inv is NULL, uses get to return the matrix and assign it to data
# then calculates the inverse of the matrix using solve and assigns it to inv

        x$setinverse(inv)
        inv
# uses setinverse to return inv
}

