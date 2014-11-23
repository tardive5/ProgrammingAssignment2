#####################################################################################################################################
## makeCacheMatrix and cacheSolve are designed to avoid the re-computation of inverted Matrix.
## A special object is created to keep the inverted matrix into a memory cache, associated to the matrix
## cacheSolve checks if the cache is virgin before computing the inverted matrix. if not, it only returns existing cache content.
#####################################################################################################################################

## MakeCacheMatrix creates a special matrix object
## PARAMETERS
## x : mandatory input parameter, Matrix type, the original matrix
##
## LOCAL VARIABLES 
## lInv : Inverted Matrix; kept in memory cache within the special object
##
## FUNCTIONS : accessors and mutators for matrix and its inverted matrix
##
## RETURN a list of functions, set, get, getInv, setInv

makeCacheMatrix <- function(x = matrix()) {
                
        ## INITIALIZATION of Inverted Matrix at object creation
        lInv <- NULL
        
        ## SET/GET FUNCTIONS for matrix
        set <- function(pNM) {
                ## PARAMETER pNM: New Matrix 
                x <<- pNM
                lInv <<- NULL
        }
        
        get <- function() x
        
        ## SET/GET FUNCTIONS for Inverted matrix
        
        setInv <- function(pInv) lInv <<- pInv
        
        getInv <- function () lInv
        
        ## RETURN the list of function references
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## cacheSolve computes the inverted Matrix
## PARAMETERS
## x : mandatory input parameter; Supposed to be special Matrix Object type
##
## LOCAL VARIABLES 
## lInv : Reference to inverted matrix of x
##
## RETURN lInv, computed here or already cached in the special Matrix Object


cacheSolve <- function(x, ...) {
        
        lInv <- x$getInv()
        
        if(!is.null(lInv)) {
                ## Already computed.
                message("getting cached Inverted Matrix")
        }
        else
        { 
                ## COMPUTE the inverted matrix of x 
                lM <- x$get()
                lInv <- solve(lM, ...)
                ## Cache the result for next computation
                x$setInv(lInv)
        }
        
        ## RETURN inverted matrix reference
        lInv
}
