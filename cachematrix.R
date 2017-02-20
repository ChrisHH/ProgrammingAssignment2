## Write a short comment describing this function

## Make cache matrix function 
## 1 initialize matrix
## 2 set parameters for matrix
## 3 returns matrix
makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {
      x <<- y
      inver <<- NULL
   }
  
  get <- function() x
  setinverse <- function(inverse) inver <<- inverse
  getinverse <- function() inver
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## Write a short comment describing this function
    
### cacheSolve function
    ## 1 check if inverse is already computed
    ## if yes: return computed inverse
    ## else: compute inverse, set as computed and return

cacheSolve <- function(x, ...) {
  inver <- x$getinverse()
  
  if(!is.null(inver)) {
    message("inverse from cache now!")
    return(inver)
    }
  
  data <- x$get()
  inver <- solve(data)
  x$setinverse(inver)
  inver
}
