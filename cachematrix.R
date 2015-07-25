## Comments are sprinkled throughout the code

## This function creates an object with the cached variables that store the matrix and its inverse as well as the set/get functions

makeCacheMatrix <- function(x = matrix()) {
  # initialize variables for the object
  minv <- NULL
  setflag <- 0 # this flag is to check if the matrix has changed
  
  # set function sets the value of the matrix
  set <- function(y) {
    # set the cached matrix and inverse
    x <<- y
    minv <<- NULL
    setflag <<- 1 # set the flag because matrix has changed and calculated inverse hasn't
  }
  get <- function() x # simple function to retrieve the stored matrix
  getflag <- function() setflag # just a check function, not essential ! 
  # set the value of the inverted matrix to the cache
  setinv <- function(solve){
    minv <<- solve
    #reset flag, important so that the get function gets value from cache now
    setflag <<- 0
  }
  # get the value from the cache or calculate, store and return 
  getinv <- function(){
    dirty <- NULL
    if(setflag ==0){ # the inverse is same as this matrix
    }else{ # set was called to set a new matrix
      dirty <- "T"
    }
    # return a list of inverse matrix and the flag
    list(minv=minv,dirty=dirty)
  }	
  # return a list of functions of the object
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv,getflag=getflag)
  
}


##Returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        
  dirty <- NULL
  inv <- x$getinv() #get the list object
  if(!is.null(inv$minv) && is.null(inv$dirty)){ # if the flag is not dirty and inverse object is not null then return from cache 
    message("inverse exists already, getting cached data")
    return(inv)
  } # else get matrix, calculate inverse and set its value to the cache
  mat <- x$get()
  m <- solve(mat)
  x$setinv(m)
  m
  
}
