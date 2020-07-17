##The general idea is that these functions show how R sets the 
##mechanism for finding free variables to retrieve them during the execution
##of a script.
##"The scoping rules of a language determine how a value is associated
## with a free variable in a function." (Peng, 2019, p.82)

##The makeCacheMatrix function creates a matrix that caches its reverse. 

makeCacheMatrix <- function(x = matrix()) {
    inv.ma <- NULL ##The function first saves inv.ma as null to be used later in the script. 
      set <- function(y) {
            x <<- y 
            inv.ma <<- NULL
      }
    get <- function()x ##The function retrieves the value of x from the parent environment.
    setinv <- function(solve.ma) inv.ma <<- solve.ma ## Here solve.ma is assigned to a free variable setted in the parent environment. 
    getinv <- function() inv.ma
    list(set = set, get = get,
         setinv = setinv, 
         getinv = getinv)
}
##The cacheSolve function execute the revers of the matrix (x)

cacheSolve <- function(x, ...){
    inv.ma <- x$getinv() ##Returns a matrix (inv.ma) that is the inverse of x
    if(!is.null(inv.ma)) {
            message("getting cached data")
            return(inv.ma)
##In case there's a cached matrix, cacheSolve will print a message 
##and will return the value of the saved matrix.
    }
    data <- x$get()
    inv.ma <- solve(data, ...)
    x$setinv(inv.ma)
    inv.ma
}

##Reference: Peng, R. D. (2019). R programming for data science. Leanpub.