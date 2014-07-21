## Stores the numeric vector
## Converts the vector into a Matrix
## Vaches its inverse

makeCacheMatrix <-  function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    dim(x) <- c(nx, ny)
    dimnames(x) <- list(row_names0, col_names0)
    rownames0 <- function(x) dimnames(x)[[1]]
    colnames0 <- function(x) dimnames(x)[[2]]
    get <- function() x
    setInverse <- function(Solve) m <<- Inverse
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## check to see if inverse value exists
## Else calsulate the inverse in cache via setInverse

cacheSolve <- function(x, ...) {
        x <- x$getInverse()
        if(!is.null(m)){
                message("Getting Stored Data")
                return(m)
        }
        data <- xget()
        m <- solve(data,...)
        x$setInverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
