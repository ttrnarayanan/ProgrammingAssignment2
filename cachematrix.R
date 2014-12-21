makeCacheMatrix <- function(x = matrix()) {
  local_m <- NULL                                         ## Initialize the local m to NULL so we can tell when cacheSolve has run at least once.  
  set <- function(y) {                                    ## Create set function to store the matrix passed in the call as x and NULL as m, both in cache.
    cache_x <<- y                                   ## Put the initial matrix from the command line into cache as cache_x
    cache_m <<- NULL                                ## Initialize caache_m to NULL so we can tell when cacheSolve has run at least once.          
  }
  get <- function() cache_x                               ## Create function to get/return the matrix passed in the command line call to '$set
  set_cache_m <- function(local_m) cache_m <<- local_m    ## Create function to set the value of cache_m in cache to the value of local_m passed in the call to '$set_cache_m.        
  get_cache_m <- function() cache_m                       ## Create function to retrieve value of cache_m from cache and return cache_m to the caller so we can check it for NULL
  list(set = set, get = get,
       set_cache_m = set_cache_m,
       get_cache_m = get_cache_m)
}

cacheSolve <- function(x) {                     ## Receive makeCacheMatrix from the caller.
  local_m<- x$get_cache_m()               ## Get the value for m in the cache environment and put it in a local m.
  if(!is.null(local_m)) {                 ## Check to see if m is NULL.  
    message("getting cached data")  ## If m is not NULL, return the value of m with a message.
    return(local_m)
  }                                       ## If we get to this line, m was NULL
  startingmatrix <- x$get()               ## Call the nested function x$get in makeCacheMatrix to obtain the UNinverted matrix with which to start, and assign it to startingmatrix.                         
  endingmatrix <- solve(startingmatrix)   ## Use solve() to invert the startingmatrix.  Assign the result to endingmatrix.
  x$set_cache_m(endingmatrix)             ## Call nested function x$set_cache_m() in makeCacheMatrix to set m in the cache environment to the local non-NULL inverted result in endingmatrix
  endingmatrix                            ## Evaluate endingmatrix so as to return it to caller/console if cache_m is non NULL.
}
