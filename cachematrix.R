## This R function was written for the second week's assignment
## for Coursera's R Programming class in June 2015.
## This function calculates the Inverse Matrix and caches it

## The first function creates a matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(solve) m <<- solve
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

## The second function inverts the matrix, 
## but checks if it has been cached first.

cacheSolve <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmean(m)
  m
        ## Return a matrix that is the inverse of 'x'
  return(m)
}

## TESTING SECTION

# s <- matrix(rnorm(1000000),1000,1000)
# t <- s
# u <- list()
# ptm <- proc.time()
# cacheSolve(makeCacheMatrix(s))
# u <- append(u, c(proc.time() - ptm))
# ptm <- proc.time()
# cacheSolve(makeCacheMatrix(t))
# u <- append(u, c(proc.time() - ptm))

