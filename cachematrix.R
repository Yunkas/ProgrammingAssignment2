## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

    
    ## this funtion creates a list of 4 functions, very identical to the example in the assignment
    
    inv_cache  <- NULL 
    
    set <- function (y){
        x <<- y 
        inv_cache <<- NULL
    }
    get <- function () x 
    setinv <- function (inv_mat) inv_cache <<- inv_mat 
    getinv <- function () inv_cache 
    list (set = set, get = get, setinv = setinv, getinv = getinv)
    
}


## Write a short comment describing this function

cacheSolve <- function(x, m=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
    
    ## Return a matrix that is the inverse of 'm'
    ## x is "special" vector, m is a matrix to solve
    ## matrix m is optional, if ommited, then, we assume the user hasn't changed the matrix and thus, we check cached inverted matrix using vector x 
    ## so the user bears the risk of not providing updated matrix. 
    
    ## first, we check if matrix m is passed along to function  
    ## if it wasn't, then, assume  matrix is the same and check if its cache inversed one exist 
    ## if it doesn't, we calculate it, assign, and return 
    ## if it does exist, then , just return it  
    
    
    inv_cache <- x$getinv()
    ma <- x$get()
    
    ## check if m is passed, i.e., its size is bigger than 1x1
    
    if (!identical (dim(m), dim (matrix()) ) ){
        ##  m isn't the same size of empty matrix , new matrix is passed
        ## DISCLAIMER: I am sure there are more elegant ways to check if parameter m is passed or not, here I imply that if m isn't specified, then m is a matrix 1x1
        
        if (is.null (inv_cache) || (!identical (ma,m))) {
            ## if at least one condition is met (either cache doesn't exist, or matrices aren't the same), we need to re-set cache & matrix 
            print ("either matrix aren't the same or cache didn't exist so recalculating inverted matrix and resetting cache")
            x$set (m)
            inv_cache <- solve (m)
            x$setinv (inv_cache)
            return (inv_cache)
        } else {print ("specified matrix is the same as it's used to be, returning cached invenrted matrix ")}
        
        
        
    } else {
        ##  m is 1, so matrix isn't passed, need to work with old matrix 
        if (!is.null (inv_cache)) {
            ## cache does exit -> return existing cached 
            print ("return cached data")
            return (inv_cache)
        } else {
            ## cache doent' exist, need to re-calculate new cache 
            print ("calculating cache as it wasn't seupt, but matrix  hasn't been changed")
            inv_cache <- solve (ma)
            x$setinv (inv_cache)
            return (inv_cache)
        }
    } ## end of else when matrix isn't passed as argument 
    
    return (inv_cache)
    
}

