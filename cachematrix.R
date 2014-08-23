## For assigment2, first i will make a matrix which is symetric. 
## when evaluated in the arguments it will have to take matrix arguments. 
## then it will cache the matrix inverse using Solve() then storing it for further calculations

## makeCacheMatrix function first defines a matrix. (arguments will define if symetric or not, for evaluation i will
## use a special matrix called toeplitz which is already on R it returns a constant diagonal matrix, only giving
## the first row... toeplitz(1:5)---> makes 5x5 matrix *** see annex comments)


makeCacheMatrix <- function(x = matrix()) {
  minverse<-NULL
  set<-function(z){
    x <<- z
    minverse<<-NULL
  }
  get<-function()x
  setsolve<-function(solve ) minverse<<-solve
  getsolve<-function() minverse
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
                
}


## cacheSolve will returns de inverse of the matrix via solve(). When evaluated has to take makeCacheMatrix as
## argument. for example: cacheSolve(makeCacheMatrix(arguments of the matrix))

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  minverse <- x$getsolve()
  if(!is.null(minverse)) {
    message("getting cached data")
    return(minverse)
  }
  data <- x$get()
  minverse <- solve(data, ...)
  x$setsolve(minverse)
  minverse
}


## annex comments: to put some "different" matrix
## toeplitz(1:5)
##      [,1] [,2] [,3] [,4] [,5]
##[1,]    1    2    3    4    5
##[2,]    2    1    2    3    4
##[3,]    3    2    1    2    3
##[4,]    4    3    2    1    2
##[5,]    5    4    3    2    1