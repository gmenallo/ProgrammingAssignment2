# The following functions 



# creates a cacheble matrix

makeCacheMatrix <- function(x = matrix()) {       
  inv <- NULL                                      # sets the initial value of the inverse matrix
  set <- function(y) {                             # sets the value of the original and inverse matrix in the global environmet  
    x <<- y
    inv <<- NULL
  }
  get <- function() x                              # creates function to return value of original matrix when called in global environment
  setsolve <- function(solve) inv <<- solve        # creates function to set value of inverse matrix when called in global environment
  getsolve <- function() inv                       # creates function to return value of inverse matrix when called in global environment
  list(set = set, get = get,                       # constructs a list of function to be called in the global environment
       setsolve = setsolve,
       getsolve = getsolve)
}





# checks if exist a cached value and return it or computes and saves a new one

cacheSolve <- function(x, ...) {                    
  inv <- x$getsolve()                              # loads inverse matrix from cache
  if(!is.null(inv)) {                              # if inverse matrix is not NULL, returns value of inv
    message("getting cached data")
    return(inv)
  }
  data <- x$get()                                  # gets original matrix from cache
  inv <- solve(data, ...)                          # computes inverse matrix
  x$setsolve(inv)                                  # saves inv in cache
  inv                                              # print inverse matrix
}
