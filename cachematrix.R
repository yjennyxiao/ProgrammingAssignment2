## makeCacheMatrix creates a special type of matrix given an input matrix x
## the build-in variables x and invMat can be changed by four build-in functions
## set, get, setInv, getInv

## cacheSolve can compute the inverse of this special type of matrix
## when the inverse matrix is cached, the cached value is produced
## otherwise a new inverse matrix is computed and cached

##================= makeCacheMatrix =================== ##
#  Description:  This function creates a special type of matrix
#  usage:   x$set(y):       change local variable x to y 
#           x$get():        return local variable x
#           x$setInv(inv):  set local inverse 
#           x$getInv():     get local inverse
#  Input:   a matrix x 
#  Output:  a special type of matrix
# ===================================================== ##

makeCacheMatrix <-function(x = matrix()){
  
  invMat <- NULL # initialize inverse matrix by null
  
  # first member function is set
  set<- function(y){
    x      <<- y
    invMat <<- NULL
  }
  
  # second member function is get
  get <- function() x
  # third member function is setInv
  setInv <- function(inv){
    if(!is.matrix(inv)) stop("input must be a matrix") 
    invMat <<- inv
  }
  # fourth member function is getInv
  getInv <- function() invMat
  
  # Create a list that contains 4 member functions. 
  # This enables x to access set, get, setInv, getInv by $ sign
  # For example:  x$get()  returns current value of x     
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}         

##================= cacheSolve ==================== ##
#  Description:  This function compute cached inverse
#  usage:   cacheSolve(x):  
#  Input:   a matrix x 
#  Output:  the inverse of x
# ================================================== ##

cacheSolve <- function(x, ...){
  invMat <- x$getInv()
  if(!is.null(invMat)){ # if value is cached, return cached value
    message("getting cached inverse matrix")
    return(invMat)
  }
  # otherwise compute inverse and cache it.
  data   <- x$get()
  invMat <- solve(data,...) # compute inverse
  x$setInv(invMat)
  invMat
}
