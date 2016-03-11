## Idea is to create a small project using the <<- operator to cache expensive operations
##

## makeCacheMatrix(standardmatrix) transforms a standard matrix into a cachesolving matrix
## Modeled on sample makeVector
makeCacheMatrix <- function(x = matrix()) {
    # input x is a standard R matrix, that should be square/invertible
    # return value are function pointers (list in R speak) to modify/solve matrix
    
    inv <- NULL
    set = function(y) {
        # use <<- to assign outside current environment
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    setinv <- function(inverse) inv <<- inverse # save the cached value
    getinv <- function() inv
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve(cachematrix) solves the inverse of x
## Modeled on sample cacheMean
cacheSolve <- function(x,...) {
        inv <- x$getinv()
        
        # if !NULL use cache
        if(!is.null(inv)) {
            message("used cached data")
            return(inv)
        }
        
        # otherwise solvea
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        return(inv)
}


## Testing ##

# 10x10 square matrix should have inverse
aMatrix = matrix(rnorm(100),nrow=10,ncol=10)
# try our new object
temp <- makeCacheMatrix(aMatrix)
# check it is the same as the original
identical(aMatrix,temp$get())
# use our new solver
cacheInv <- cacheSolve(temp)
# check new solver matches the solve of the original
identical(solve(aMatrix),cacheInv)