makeCacheMatrix <- function(x = matrix()) {
  m_inv <- NULL
  get <- function() x
  setinv <- function(inverse) m_inv <<- inverse
  getinv <- function() m_inv
  list(get = get,
       setinv = setinv,
       getinv = getinv)
}


cacheSolve <- function(x, ...) {
  m_inv <- x$getinv()
  if(!is.null(m_inv)) {
    message("getting cached data")
    return(m_inv)
  }
  data <- x$get()
  m_inv <- solve(data, ...)
  x$setinv(m_inv)
  m_inv
}
