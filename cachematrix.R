## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# rwy: makeCacheMatrix -- This function takes and caches two matrices for later optimized use
# rwy: retObject -- Is a list which contains useful cache-functions  
# rwy: retObject <- makeCacheMatrix(<matrix>) -- This is a constructor function, the original matrix can be set by an argument to the ... 
# rwy: retObject$set(<a matrix>)  -- ... function or the "set" function; so, the original matrix will be cached, too 
# rwy: retObject$get()  -- ... gets back the original cached matrix
# rwy: retObject$setInverseMatrix(<matrix>)  -- caches the [inverse/any] matrix
# rwy: retObject$getInverseMatrix()  -- returns the cached [inverse/any] matrix

## For test purposes
# rwy: TestMatrix <- matrix(c(2,3,21,22,5,7,9,11,13,17,23,41,52,23,61,67),nrow=4, ncol=4)
# rwy: solve(TestMatrix)
# rwy: E <- round(InverseTestMatrix %*% TestMatrix) # One-Matrix

makeCacheMatrix <- function(x = matrix()) {
      inverseMatrix <- NULL         # reset variable for the inverse matrix  
      set <- function(y) {          
            x <<- y                 # set-function keeps original matrix and ...
            inverseMatrix <<- NULL  # ... resets variable for inverse matrix
      }
      get <- function() x           # return the original matrix
      setInverseMatrix <- function(Inverse) inverseMatrix <<- Inverse # sets abd keeps the inverse matrix 
      getInverseMatrix <- function() inverseMatrix                    # returns the cached inverse matrix
      # return a list with the exposed functions  
      list(set = set, get = get,setInverseMatrix = setInverseMatrix, getInverseMatrix = getInverseMatrix)
}


## Write a short comment describing this function

# rwy: cacheSolve -- If available, this function reads the cached inverse matrix and returns it.
# rwy:            -- Otherwise the inverse matrix will be created and kept/cached for future purposes. 
# rwy: invMatrix  -- Is a list which contains useful cache-functions  
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      iMatrix <- x$getInverseMatrix()   # Get cached inverse matrix
      if(!is.null(iMatrix)) {           # If it is available (iMatrix is NOT NULL), return it from cache ...
            message("getting cached inverse matrix")
            return(iMatrix)             # ... and return inverted Matrix
      }
      originalMatrix <- x$get()         # Otherwise get original matrix ...
      iMatrix <- solve(originalMatrix)  # ... inverse it and ...
      x$setInverseMatrix(iMatrix)       # ... keep it in cache
      iMatrix                           # return inverted Matrix 
}
