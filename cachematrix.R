## Diego, 2014.

## MAKECACHEMATRIX and CACHESOLVE pair up together to make getting the inverse of a matrix an efficient process.

## MAKECACHEMATRIX(X) initializes a "super"-matrix XX from the original X by defining its caching methods for 'setting' and 'getting' itself and its inverse.
makeCacheMatrix <- function(X=matrix( )){
    INV <- NULL
    setSelf    <- function(Y){
        X   <<- Y
        INV <<- NULL}
    getSelf    <- function( ){X}
    setInverse <- function(I){
        INV <<- I}
    getInverse <- function( ){INV}
    # The list of methods make X a "super"   XX.
    return(list(setSelf=setSelf,
                getSelf=getSelf,
                setInverse=setInverse,
                getInverse=getInverse))}


## CACHESOLVE(XX) gets the inverse of the "super"-matrix XX, and hence of original X.  In order to do this, it checks if it has been calculated and cached, if not it then calculates and caches it.
cacheSolve <- function(XX, ...){
    INV <- XX$getInverse( )
    if(!is.null(INV)){
        name <- deparse(substitute(XX))
        mssg <- paste("I found the inverse of %s in the ",
            "cache... getting it for you.")
        message(sprintf(mssg, name))
        return(INV)}
    else{
        name <- deparse(substitute(XX))
        mssg <- paste("I don't have the inverse of %s in the",
            "cache... it might take a bit longer to calculate",
            "it.")
        message(sprintf(mssg, name))
        X   <- XX$getSelf( )
        INV <- solve(X, ...)
        XX$setInverse(INV)
        return(INV)}}

