## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv.mat <- NULL
  set <- function(y) {
    x <<- y
    inv.mat <<- NULL
  }
  get <- function() x
  setinvmat <- function(inverse) inv.mat <<- inverse
  getinvmat <- function() inv.mat
  list(set=set, get=get, setinvmat=setinvmat, getinvmat=getinvmat)
}


# cacheSolve function returns the inverse of the matrix. Function checks if
# the matrix is already present in cache with !is.null(inv.mat) stetment. 
# If inv.mat is not null results are picked from cache
# if matrix is null than this function compute the inverse and stores result in cache by calling setinvmat function.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv.mat <- x$getinvmat()
  if(!is.null(inv.mat)) {
    message("getting cached data.")
  }
  else{
   data <- x$get()
   inv.mat <- solve(data)
   x$setinvmat(inv.mat)
 }
  return(inv.mat)
}

# create a matrix with ramdom values of specified columns and rows
NCols <- 5 
NRows <- 5
# A matrix with random integer is created here 
myMat<-matrix(round(runif(NCols*NRows, min = 1, max = 10)), nrow = NRows, ncol = NCols) 

#cache matrix
cacheMat <- makeCacheMatrix(myMat)

#Check matrix 
cacheMat$get()
# Note that matrix will be diffrent each time as random integer are generated to create matrix
#> cacheMat$get()
#[,1] [,2] [,3] [,4] [,5]
#[1,]    9    9    8    6    1
#[2,]    6    6   10    9    3
#[3,]    9    7    5    5    6
#[4,]    1    4    4    7   10
#[5,]    7    7    3    9    7
#call function cacheSolve to check 
invMat <- cacheSolve(cacheMat)
invMat
#> invMat <- cacheSolve(cacheMat)
#> invMat
# First call print statement is not called
#[,1]        [,2]        [,3]        [,4]         [,5]
#[1,] -0.20216512  0.10499543  0.23255511 -0.17829660  0.039259163
#[2,]  0.39167862 -0.27116212 -0.22668580  0.18220947 -0.005738881
#[3,] -0.01212991  0.10629973  0.07395331  0.04930220 -0.177644450
#[4,] -0.08999609  0.11125603 -0.12873353 -0.08582236  0.198121821
#[5,] -0.06860571 -0.02243381  0.12795096  0.08530064 -0.069257858

## next time getting cached data statement is printed
#> cacheSolve(cacheMat)
#getting cached data.
#[,1]        [,2]        [,3]        [,4]         [,5]
#[1,] -0.20216512  0.10499543  0.23255511 -0.17829660  0.039259163
#[2,]  0.39167862 -0.27116212 -0.22668580  0.18220947 -0.005738881
#[3,] -0.01212991  0.10629973  0.07395331  0.04930220 -0.177644450
#[4,] -0.08999609  0.11125603 -0.12873353 -0.08582236  0.198121821
#[5,] -0.06860571 -0.02243381  0.12795096  0.08530064 -0.069257858

#Check if inverse is correct
checkInverse <- solve(myMat)
checkInverse
#> checkInverse <- solve(myMat)
#> checkInverse
#[,1]        [,2]        [,3]        [,4]         [,5]
#[2,]  0.39167862 -0.27116212 -0.22668580  0.18220947 -0.005738881
#[3,] -0.01212991  0.10629973  0.07395331  0.04930220 -0.177644450
#[4,] -0.08999609  0.11125603 -0.12873353 -0.08582236  0.198121821
#[5,] -0.06860571 -0.02243381  0.12795096  0.08530064 -0.069257858

identical(invMat, checkInverse)
#> identical(invMat, checkInverse)
#[1] TRUE
 

