makeCacheMatrix <- function(x = matrix()) { ##Creates a "matrix" object that can cache its inverse.

        mInv <- NULL
        set <- function(y) {
                x <<- y
                mInv <<- NULL
        }
        ## get the value of the matrix we input.
        get <- function() x
       
        ## set the value of the matrix(save the inverse matrix).
        setmInv <- function(i) mInv <<- i
        
        ## get inverse matrix (getting cached data > which we alredy save by useing setmInv in cacheSolve )
        getmInv <- function() mInv
        
        list(set = set, get = get,
             setmInv = setmInv,
             getmInv = getmInv)
}


cacheSolve <- function(x, ...) {## Return the inverse of 'x' (matrix)
        mInv <- x$getmInv()	
        
        if(!is.null(mInv)) {    
        ##the inverse has already been calculated (and not changed), so we should retrieve the inverse from the cache.
                print("getting cached data")
                return(mInv)
        }
        
        ## if not(the inverse is not calculated yet)--> get the inverse
        data <- x$get() ##get matrix
        mInv <- solve(data, ...) ##calculate the inverse 
        ## after we get the inverse, we set the inverse.
        x$setmInv(mInv)
        ##return the inverse
        mInv
}
