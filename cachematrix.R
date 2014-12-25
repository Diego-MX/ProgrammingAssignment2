## Diego, 2014.

## MAKECACHEMATRIX and CACHESOLVE pair up together to make getting 
# the inverse of a matrix an efficient process.

## MAKECACHEMATRIX(X) initializes a cachematrix CMAT from the "regular" 
# matrix MAT by defining its caching methods for 'setting' and 'getting' 
# itself and its inverse.
makeCacheMatrix <- function(mat=matrix( )){
    inv <- NULL
    
    setSelf    <- function(m){
        mat <<- m
        inv <<- NULL
        return( )}
    getSelf    <- function( ){mat}
    
    setInverse <- function(i){
        inv <<- i
        return( )}
    getInverse <- function( ){inv}

    # This list of methods is what make MAT a cache CMAT.
    methods <- list(setSelf   =setSelf,    
                    getSelf   =getSelf,
                    setInverse=setInverse,
                    getInverse=getInverse)
    return(methods)}


## CACHESOLVE(CMAT) gets the inverse of the cached-matrix CMAT, and 
# hence of original MAT.  In order to do this, it checks if it has been 
# calculated and cached, if not it then calculates and caches it.
cacheSolve <- function(cmat, ...){ 
    inv_ <- cmat$getInverse( )  # Check if INV_ is inverse.
    if(!is.null(inv_)){
        name   <- deparse(substitute(cmat))   # Variable name for communication purposes.
        msgtmp <- paste("I found the inverse of `%s` in the ",
            "cache... getting it for ya.")
        message(sprintf(msgtmp, name))
        
        inv  <- inv_            # INV_ being inverse confirmed.
        return(inv)}
    
    else{
        name   <- deparse(substitute(cmat))
        msgtmp <- paste("I don't have the inverse of `%s` in the",
            "cache... gonna calculate it for ya.")
        message(sprintf(msgtmp, name))
        
        mat  <- cmat$getSelf( ) # Use library function SOLVE on matrix MAT to get inverse. 
        inv  <- solve(mat, ...)
        cmat$setInverse(inv)
        return(inv)}}

