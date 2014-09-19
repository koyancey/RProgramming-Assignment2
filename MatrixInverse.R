makeCacheMatrix <- function(x = matrix()) { 
  
  matInv <- NULL                     
  set <- function(y) {                      
    x <<- y
    ## change the value of inverse of the matrix in case the matrix was changed.
    matInv <<- NULL              
  }
  ## gets the value of the inverse
  get <- function() x                           
  
  setInv <- function(solve) matInv <<- solve 
  getInv <- function() matInv 
  list(set = set, get = get,                    
       setInv= setInv,
       getInv = getInv)
}

cacheSolve<- function(x, ...) {                 
  matInv <- x$getInv()
    if(!is.null(matInv )) {                 
    message("getting cached data - Inverse of the matrix")
    return(matInv )
  }
  data <- x$get()                               
  matInv <- solve(data, ...)
  x$setInv (matInv )
  matInv 
}
