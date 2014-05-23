## Functions allow to compute the inverse of a matrix and cache the result

# Function makeCacheMatrix creates a matrix object, allows to get matrix, 
# get and set matrix inversion

makeCacheMatrix <- function(x = matrix()){
    solve <- NULL
    set <- function(y){
        x <<- y
        solve <<- NULL
    }
    get <- function(){
        x
    }
    setSolve <- function(s){
        solve <<- s
    }
    getSolve <- function(){
        solve
    }
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


# Function computes matrix inversion. If the inversion was already computed
# function returns the cache inverted matrix

cacheSolve <- function(x, ...){
    s <- x$getSolve()
    if(!is.null(s)){
        message("Getting cached data")
        return(s)
    }
    matrix <- x$get()
    s <- solve(matrix)
    x$setSolve(s)
    s
}