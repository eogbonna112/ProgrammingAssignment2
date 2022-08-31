## Put comments here that give an overall description of what your
## functions do ## Edmond Ogbonna eogbonna112

## Write a short comment describing this function
library(datasets) ##load the data set to use
data(mtcars) ##load the mtcars data set
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set <-function(y) {
                    x<<-y    
                    inv<<-NULL ## inverse as NULL
  }
  get <-function()x  ##function to get matrix x
  setinv <-function(inverse)inv<<-inverse
  getinv <-function(){
                     inver <-ginv(x)
                     inver%*%x  ## function to obtain inverse of the matrix
                     }
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) ##gets cache data
  {
  inv <-x$getinv()
  if(!is.null(inv)){
                    message("getting cached data!")
                    return(inv)  ## returns inverse value
  }
  data <-x$get()
  inv <-solve(data,...)
  x$setinv(inv)
  inv ## Return a matrix that is the inverse of 'x'
}