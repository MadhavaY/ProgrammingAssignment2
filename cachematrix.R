\## Functions to create a matrix that caches its inverse.

## Creates a special "matrix" object that caches the inverse of
## a matrix.
##
## Object has members:
##	- get() - Gets the value of the matrix
##	- set() - Sets the value of the matrix
## 	- getinverse() - Gets the matrix inverse
##	- setinverse() - Sets the matrix inverse


makeCacheMatrix <- function(x = matrix()) 
{
	# holds the actual matrix's inverse
	inv <- NULL
	
	# sets actual matrix x and NULLs cached inverse
	set <- function(newMatrix)
	{
		x <<- newMatrix
		inv <<- NULL	
	} 

	# returns actual matrix x
	get <- function() x
	
	setinverse <- function(minverse) inv <<- minverse
	getinverse <- function() inv

	# put it all together...
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## Computes and returns the inverse of a special "matrix"
## created with makeCacheMatrix

cacheSolve <- function(x, ...) 
{
	inv <- x$getinverse()
	if (!is.null(inv))
	{
		message("getting cached inverse")
		return(inv)
	}

	# cache miss, compute and return inverse of x
	actual <- x$get()
	inv <- solve(actual)
	x$setinverse(inv)
	inv
}
