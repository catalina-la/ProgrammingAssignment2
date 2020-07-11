####Assignment Week 3####

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#MakeCacheMatrix is a function that takes as an argument a matrix. It creates a
#matrix called m, which stores a cache of the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL #we define a empry object
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse #we calculate the inverse of m
  getinverse <- function() m #we obtain the inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) #we store the values obtained before in a list
}


## Write a short comment describing this function
#CacheSolve is a function is a function that estimates the inverse of the matrix 
#m, which was the one we created with the previous function
cacheSolve <- function(x, ...) {
  m <- x$getinverse() #Obtain the matrix inverse value
  if(!is.null(m)) { #if m isnt empty, it displays the value stored on the cache
    message("getting cached data")
    return(m)
  }
  matriz <- x$get() #if the inverse value isnt found on the cache, it calculates 
  #the inverse
  m <- solve(matriz, ...)
  x$setinverse(m)
  m
}

#check
m<- makeCacheMatrix(matrix(1:4,2,2))
cacheSolve(m)
cacheSolve(m)
