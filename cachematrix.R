## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
 
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setminverse <- function(minverse) m <<- minverse
    getminverse <- function() m
    list(set = set, get = get,
         setminverse = setminverse,
         getminverse = getminverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getminverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setminverse(m)
  m
}


# > newMat<-makeCacheMatrix(MyMatrix)
# > cacheSolve(newMat)
# [,1]         [,2]         [,3]
# [1,]  0.10281871 -0.014413837 -0.003096306
# [2,] -0.05573350  0.004697843  0.018293117
# [3,]  0.01313261  0.019965834 -0.005587586
# > cacheSolve(newMat)
# getting cached data
# [,1]         [,2]         [,3]
# [1,]  0.10281871 -0.014413837 -0.003096306
# [2,] -0.05573350  0.004697843  0.018293117
# [3,]  0.01313261  0.019965834 -0.005587586
