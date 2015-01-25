## Below functions are defined to show concept of lexical scoping.How we can take advantage of the scoping rules.
## Computation of matrix Inverse is potentially time-consuming. 
## Below functions are used to compute inverse matrix, save inverse in cache and read from cache if same inverse (if matrix content not changed) needed to compute repeatedly
## Inverse get retreived from cache rather than recomputed.  of the scoping rules of the R language and how they can be manipulated to preserve state inside of an R object

## The makeCacheMatrix function create special matrix, set and get value of matrix
## Set and Get Inverse of Matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(solve) m <<- solve
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## The cacheSolve function computes inverse of the special "matrix" created with the above function. 
## It first checks if the inverse has already been computed.
## If yes, it gets the inverse from the cache. Otherwise, it computes inverse

cacheSolve <- function(x, ...) {

  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...) ## Return a matrix that is the inverse of 'x'
  x$setInv(m)
  m
}
