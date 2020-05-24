## The makeCacheMatrix() and cacheSolve() functions are used to cache the inverse
## of the matrix to accelarate the calculations by storing the value in the memory (caching).

## makeCacheMatrix() function creates an object of makeCacheMatrix type by returning 
## a list containing the getter and setter functions defined inside the function.

makeCacheMatrix <- function(x = matrix()) {
    # assign the default NULL value to variable i which store the inverse of the matrix
    i <- NULL
    # set() function is designed to overwrite existing matrix. 
    # It takes one argument (type matrix) and assign it to variable x while
    # setting inverse i to NULL to prevent returning incorrect inverse
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    # get() function returns stored matrix
    get <- function() x
    # setinv() functions assigns calculated inverse to variable i
    setinv <- function(inv) i <<- inv
    # getinv() function returns the calculated inverse
    getinv <- function() i
    # give the corresponding names to functions defined above - this allow
    # to use the extract operator ($) instead of double bracket notation ([[]])
    list(set = set, get = get,
         setinv = setinv, 
         getinv = getinv)
}


## cacheSolve() function populate and/or retrieve the inverse 
## from makeCacheMatrix objects

cacheSolve <- function(x, ...) {
    # get the inverse function stored in makeCacheMatrix object 
    # and assign it to variable i
    i <- x$getinv()
    # if variable i already stores a matrix inverse, return its value
    if(!is.null(i)) {
        message('getting cached inverse')
        return(i)
    }
    # get the matrix stored in makeCacheMatrix object 
    # and calculate the matrix inverse with solve() function
    data <- x$get()
    i <- solve(data)
    # set and store the calculated inverse in makeCacheMatrix
    x$setinv(i)
    # return the calculated inverse
    i
}
