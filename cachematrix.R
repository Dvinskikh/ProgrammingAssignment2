makeMatrix <- function(x = matrix())
{
  ##variable for caching Solve() result
  cache <- NULL
  
  ##sets new matrix value:
  set <- function(y)
  {
    x <<- y
    cache <<- NULL
  }
  
  ##returns matrix:
  get <- function() x
  
  #sets new cache value:
  set_cache <- function(new_val) cache <<- new_val
  ##returns cache value:
  getcache <- function() cache
  
  list(set = set, get = get,
       set_cache = set_cache,
       getcache = getcache)
}

cacheSolve <- function(x, ...) {
  ##getting old cache value
  cache <- x$getcache()
  ##check if old cached value is not NULL
  if(!is.null(cache))
  {
    message("getting cached data")
    return(cache)
  }
  
  data <- x$get()
  ##calculating new value to cache
  cache <- solve(data, ...)
  ##updating cache
  x$set_cache(cache)
  cache
}
