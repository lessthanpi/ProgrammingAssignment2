library(matlib)

## function builds cache tools into a list
makeCacheMatrix <- function(x = matrix()) {
  inverse_mat <- NULL
  ## input setter
  set <- function(y) 
    {      
    x <<- y
    inverse_mat <<- NULL
    }
  ## input getter
  get = function() x
  ## output setter
  setInv = function(inverse) inverse_mat <<- inverse
  ## output getter
  getInv = function() inverse_mat
  
  ##cacheSolve uses this list to compute
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}

cacheSolve <- function(x, ...) 
  {
  ## pulls whatever is cached (if anything)
  inverse_mat <- x$getInv()
  if(!is.null(inverse_mat)) ##returns already completed inv if possible
    {
    message("getting cached data")
    return(inverse_mat)
    }
  ##calls inverse creation method in other function
  data <- x$get() 
  ##solves for the inverse
  inverse_mat <- solve(data, ...)
  ##stores in cache
  x$setInv(inverse_mat)
  ##returns the completed inverse
  return(inverse_mat)
}

#sample code to test, works fine!
mat <- matrix(1:4,nrow=2,byrow=T)
list_from_makeCacheMatrix <- makeCacheMatrix(mat)
cacheSolve(list_from_makeCacheMatrix)
