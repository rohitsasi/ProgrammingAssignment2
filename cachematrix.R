## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	## @x: a square invertible matrix
        ## below capabilities are done by the functions
        ##              1. set the matrix
        ##              2. get the matrix
        ##              3. set the inverse
        ##              4. get the inverse
        ##         this list is used as the input to cacheSolve()
        
        inv = NULL
        set = function(y) {
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv = x$getinv()
        
        # if the inverse previously calculated
        if (!is.null(inv)){
                message("receiving cashed data")
                return(inv)
        }
        
        # else, the inverse is computed
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        # the setinv function assigns the value of the inverse in the cache
        x$setinv(inv)
        
        return(inv)
}
