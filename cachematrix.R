## The overall intent of these combined functions is to reduce computational
## stress in calculating the inverse of a matrix multiple times via the use
## of cached objects.

makeCacheMatrix <- function(baseMtx = matrix(), ...) {
	## Creates a special matrix object that is able to cache its inverse
	## by creating a list to
	## 1. Set the value of an input matrix
	## 2. Get the value of the given matrix
	## 3. Set the value of the inverse of the matrix given
	## 4. Get the value of the inverse of the input matrix	

	mtxInverse <- NULL  ## Clear cache

	set <- function(y) {
		baseMtx <<- y
		mtxInverse <<- NULL
		}  
	## in global environment sets defined matrix as input and clears any 
	## cached inverse
	
	get <- function() baseMtx ## a function to retrieve the input matrix

	setinverse <- function(solve) mtxInverse <<- solve 
	##calculates inverse based on an argument called "solve"	

	getinverse <- function() mtxInverse 
	## defined function to return the value of an already cached matrix 
	## inverse

	list(set = set, get = get, 
	     setinverse = setinverse, 
	     getinverse = getinverse)
}

cacheSolve <- function(baseMtx, ...) {
	## Calculate inverse of defined Matrix, if it already exists, return
	## from cache

	mtxInverse <- baseMtx$getinverse() 
	## set value based on getinverse function defined when running 
	## makeCacheMatrix

	if(!is.null(mtxInverse)) {
		message("Retrieving data from cache")
		return(mtxInverse)
	} 
	## checks if the object returned from getinverse exists (i.e. if it 
	## has previously been calculated). If so, it will return this value, 
	## avoiding re-calculation, if not the function continues to execute.

	data <- baseMtx$get() 
	## retrives base matrix
	mtxInverse <- solve(data, ...) 
	## calculates the inverse of the base matrix
	baseMtx$setinverse(mtxInverse) 
	## sets the calculated inverse into the cache for later recall 
	mtxInverse 
	## prints calculated inverse
}
