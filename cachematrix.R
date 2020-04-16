#Two functions outlined below:
#First one defines a set of functions (getters and setters) for a matrix and its inverse values
#Second function retrieves the cached result from the environment of first function
#or calculates it and sets the result in that environment so it doesn't need to be calculated again


#makeCacheMatrix lists four functions to:
# Set and Get a matrix object and its inverse
# Initialises the environment where results can be stored

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    #set assumed matrix y to x. clear any previous m value 
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    #get x from the parent environment
    get <- function() x
    #setter and getters for the matrix inverse
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    #put functions into a list which can be called from parent environment
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse #e.g. sets the name "getinverse" for "getinverse()" that was defined
         )
}

#cacheSolve returns the inverse of matrix 'x' in two ways:
#Check if the inverse has already been calculated and cached
#If not calculated or different, calculate it in the function, cache and return it

cacheSolve <- function(x, ...) {
    #If matrix already calculated/set, retrieve it
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    #If matrix has changed, or not calculated then calculate it
    data <- x$get()
    m <- solve(data, ...)
    # cache the result so it can be later retrieved without calculation
    x$setinverse(m)
    m
}

