## These functions are able to compute the inverse of a matrics and
## store it to cache. The next time an inverse is to be computed,
## it is first checked whether the inverse already exists and it is
## only recomputed if the inverse does not yet exist.

## In makeCacheMatrix, a list is created which is used for inserting
## and outputting a matrix and its corresponding inverse. 

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL # set m equal to zero in the function environment
      
      ## Define the set function
      set <- function(y) { 
            x <<- y # set x equal to y in the parent environment
            m <<- NULL # set m equal to NULL in the parent environment
      }
      
      ## Define the get function
      get <- function() x
      
      ## Define the setinverse function which takes the solve function as  
      ## its argument and sets 'm' equal to 'solve' in the parent environment
      setinverse <- function(solve) m <<- solve
      
      ## Define the getinverse function
      getinverse <- function() m
      
      ## List the outcomes
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## This function chechks whether the inverse of a matrix
## already exists in cache. If so, it loads the results.
## If the inverse does not exist yet, it is computed.

cacheSolve <- function(x, ...) {      
      # set m equal to the inverse matrix if it exists
      m <- x$getinverse()
      
      # If m is not NULL, than use the cached data
      if(!is.null(m)) {
            message("getting cached data")
            return(m) # returns 'm' and makes sure that 
            # the rest of the function is not 
            # evaluated
      }
      
      # if m equals NULL than the inverse is to be calculated
      data <- x$get() # assign the matrix to the variable 'data'
      m <- solve(data, ...) # calculate the inverse matrix and 
      # assign it to 'm'
      x$setinverse(m) # assign the resulting matrix 'm' to 
      # the 'setinverse' element of 'x'
      m # return 'm' 
}
