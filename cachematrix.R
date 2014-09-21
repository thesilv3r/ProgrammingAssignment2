## The overall intent of these combined functions is to reduce computational
## stress in calculating the invere of a matrix multiple times via the use
## of cached objects.

makeCacheMatrix <- function(baseMtx = matrix(), ...) {
	## Creates a special matrix object that is able to cache its inverse by
	## creating a list to
	## 1. Set the value of an input matrix
	## 2. Get the value of the given matrix
	## 3. Set the value of the inverse of the matrix given
	## 4. Get the value of the inverse of the input matrix	

	mtxInverse <- NULL  ## Clear cache

	set <- function(y) {
		baseMtx <<- y
		mtxInverse <<- NULL
		}
	
	get <- function() baseMtx

	setinverse <-    function(solve)
			 mtxInverse <<- solve

	getinverse <- function() mtxInverse

	list(set = set, get = get, 
	     setinverse = setinverse, 
	     getinverse = getinverse)
}

cacheSolve <- function(baseMtx, ...) {
	## Calculate inverse of defined Matrix, if
	## already exists, return from cache

	mtxInverse <- baseMtx$getinverse()

	if(!is.null(mtxInverse)) {
		message("Retrieving data from cache")
		return(mtxInverse)
	}

	data <- baseMtx$get()
	mtxInverse <- solve(data, ...)
	baseMtx$setinverse(mtxInverse)
	mtxInverse
}
