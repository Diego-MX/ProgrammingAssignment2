## Diego, 2014.

## MAKECACHEMATRIX and CACHESOLVE pair up together to make getting
## the inverse of a matrix an efficient process.

## MAKECACHEMATRIX(X) initializes the matrix X by defining its
## caching methods of 'setting' and 'getting' itself and its inverse.
makeCacheMatrix <- function(X=matrix( )){
    INV <- NULL
    setMatrix  <- function(Y){
        X   <<- Y
        INV <<- NULL}
    getMatrix  <- function( ){X}
    setInverse <- function(I){
        INV <<- I}
    getInverse <- function( ){INV}
    # Returns -or stores- its list of methods.
    list(setMatrix=setMatrix,
        getMatrix=getMatrix,
        setInverse=setinverse,
        getInverse=getinverse)}

## CACHESOLVE(X) gets the inverse of the matrix X by first checking
## if it has been calculated already, or otherwise calculating it.
cacheSolve <- function(X, ...){
    INV <- X$getInverse( )
    if(!is.null(INV)){
        name <- deparse(substitute(X))
        sentence <- "I found the inverse of %s in the cache... getting it for you."
        message(sprintf(sentence, name))
        return(INV)}
    else{
        X_  <- X$getMatrix( )
        INV <- solve(X_, ...)
        X$setInverse(INV)
        return(INV)}}

