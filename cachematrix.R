## This function creates a special "matrix" object that can cache its inverse
## If the contents of your matrix are not changing, use this function to 
## create its inverse ans store on cache, saving time and computer resources
makeCacheMatrix <- function(x = matrix()) {
        inversematrix <- NULL 
        ## Creates a empty variable called inversematrix
        set <- function (y){
                x <<- inversematrix
                ## Sets inverse matrix
                inversematrix <<- NULL
                ## Clear variable
        }
        get <- function() x
        ## Gets matrix 
        setsolve <- function(solve) inversematrix <<- solve
        getsolve <- function() solve
        list(set = set, get = get,
             setsolve = setsolve, 
             getsolve = getsolve)
        ## functions are executed and returned into a list
}

## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix function
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        inversematrix <- x$getsolve()
        ## Inversematrix takes a specific value of x
        if(!is.null(inversematrix)){
                message("Getting cached data")
                return(inversematrix)
                ## If inverserse matrix are NOT empy is because solve was
                ## calculated by makeCachematrix, so prints its values
        }
        data <- x$get()
        inversematrix <- solve(data, ...)
        ## setting solve to cache
        x$setsolve(inversematrix)
        inversematrix
        ## inversematrix are returned
}
