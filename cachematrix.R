##  makeCacheMatix  -  cache matrix inverse
##  Returns a list of functions to cache and return 
##       the inverse of a matrix, "x".
##  
##  Access the functions as follows  
##    var <-makeCacheMatrix(x)
##    var$get()               Get the original matrix
##    var$set(mat)            Set a new value for original matrix
##    var$getInvt()           Get the inverse matrix
##    var$setInvt(invx)       Set the invert matrix
##  
##  x parameter is the invertable matrix
##       passed by value and a variable of the same name, type, and value
##       is create in "makeCacheMatrix" environ
makeCacheMatrix <- function(x = matrix()) {
      ##  x parameter is the invertable matrix
      ##       passed by value and a variable of the same name, type, and value
      ##       is create in "makeCacheMatrix" envir
      ##  x will be calculated and cached only if needed
      invx <- NULL
      
      ##  makecCacheMatrix function defines the parent environment 
      ##    for all the locally defined getter and setter functions.
      
      ##  First getters and setters for the maxrix
      set <- function( newx ) {
            ##  update x defined in the parent environment
            ##  <<-  will search parent envs for x
            invx <<- NULL
            x <<- newx
      }
      
      #  Returns the current maxtrix 
      get <- function() {
            ##  return the matrix defined in parent environment
            ##  will search parent environments for x
            x
      }
      
      ##  Inverse matrix getters and setters
      ##  Set the inverse matrix
      setInv <- function(newInv){
            ##  update inverse of current matrix in parent env
            invx <<- newInv
      }
      
      #  Return the current inverse matrix
      getInv <- function(){
            ## like the getter above, will search the parent env for inverse matrix
            invx
      }
      
      ##  Return list of functions
      list(set = set, 
           get = get,
           setInv = setInv,
           getInv = getInv
      )
      
}

##  cacheSolve - return matrix inverse of x
##    where x parameter is the function list returned by "makeCacheMatrix"
##  mat <- invertable matrix
##  x <- makeCacheMatrix(mat)
##  invx <- cacheSolve(x)
##
##  makeCacheMatrix must be called first with the original matrix as arguement

cacheSolve <- function(x, ...) {
      
      
      ##  Check to see if matrix inverse exists
      invx <- x$getInv()
      
      #  Will return NULL if the Matrix Inverse has not been cached
      if(!is.null( invx )) {
            ## message("getting cached data")
            return( invx )
      }
      
      ##  Inverse Matrix not cached
      ##  get matrix to invert
      data <- x$get()
      
      ##  the "solve" function with only one arguements returns the inverse matrix
      x$setInv( solve(data) )  ##  save the inverse in the cache
}
