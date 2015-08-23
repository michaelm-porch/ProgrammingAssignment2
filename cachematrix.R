setwd("~/dev/Coursera/RProgramming/")
## These functions will optimize the calculation of the inverse of a sqaure matrix by taking advantage of the 
## scoping rules of the R language to manipulate and preserve state. Specifically, they will create a cache 
## object at he highest scope level and manage that cache for each anonimous function within the main 
## function body.

## makeCacheMatrix is a container function that returns a list of inner functions that get and set a matrix 
## in local cache. makeCacheMatrix is optimized to manage and make use of a cache object that greatly 
## improves the retrieval performance of the getMatrix function.
makeCacheMatrix <- function(x = matrix()) {
  # local cache object that stores the matrix 
  localCache <- NULL

  #set function
  #assign matrix to local variable
  #clear localCache anything currently stored will be invalid at this point
  set <- function (y) {
    x <<- y
    localCache <<- NULL
  }
  
  #get function
  #return the the matrix stored in the parent container 
  get <- function() {
    return (x)
  }
  
  #setMatrix function
  #Assign the matrix to the localCache
  setMatrix <- function(mtx) localCache <<- mtx 
  
  #getMatrix function
  #retrieve the localCache - this is the performance boost!
  getMatrix <- function() {
    return (localCache)
  }

  #list inner functions
  list(set = set, get = get, setMatrix = setMatrix, getMatrix = getMatrix)
}


## cacheSolve calculates the inverse of a square matrix and is optimized for performance by taking
## advantage of the cached value stored by the makeCacheMatrix function API. 
cacheSolve <- function(x, ...) {
  # fetch the cached matrix (created by makeCacheMatrix) if it exists
  cacheMatrix <- x$getMatrix()

  #return the matrix straight away if it exists
  if (!is.null(cacheMatrix)) {
    message("cache hit!")
    return (cacheMatrix)
  }
  
  # git here then the cache was empty so call the function to get the source matrix
  mtx <- x$get()

  # solve(a) throws anexception if the matrix a is not square so that neds to be handled
  tryCatch (
    {
      cacheMatrix <- solve(mtx, ...)
    },
    error = function(e) {
      message("ERROR - solve(x, ...)")
      message(e)
      
      return (NA)
    },
    warning = function(e) {
      message("solve(x, ...)")
      message(e)
      
      return (NA)
    },
    finally = {
      #good to go - set the cache value
      x$setMatrix(cacheMatrix)
    }
  )
  
  return (cacheMatrix)
}

