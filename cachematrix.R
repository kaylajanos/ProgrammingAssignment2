## The following code can be used to:
## A) Create and store the inverse of a matrix 
## B) Call a matrix, determine if its inverse has been computed and/or stored, then either compute the inverse or call the stored one 


## The following fuction helps to create the matrix and can be used to store its inverse 
makeCacheMatrix <- function(x = matrix()) {  
  inv <- NULL #Initializing where the inverse will be stored
  setX<- function(y) { #Sets the matrix that will be worked with 
    x <<- y
    inv <<- NULL
  }
  getX <- function() x #Simply grabbing the matrix 
  setInverse <- function(inverse) inv <<- inverse #Fuction to inverse the matrix
  getInverse <- function() inv #Getting in the inverse
  #Listing the functions so that they can used after makeCacheMatrix initializes X 
  list(setX = setX, getX = getX,
       setInverse = setInverse,
       getInverse = getInverse)
}



#The following function calculates the inverse of the matrix that 
# was created with the above function. However, it first checks to see if the
#inverse has already been calculated. If so, it `get`s the inverse from the
#cache and skips the computation. Otherwise, it calculates the mean of
#the data and sets the value of the mean in the cache via the `setmean`
#function.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("Grabbing your stored/cached data for you!")
    return(inv)
  }
  data <- x$getX()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}

c