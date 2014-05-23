## This function will create a special kind of matrix that can hold
## computations done for getting the inverse.
##
## Use:
## x <- makeCacheMatrix(mat)
##
## x$set(mat) will change the matrix contained in x
## x$get() will display the matrix contained in x
## x$getsolve() and x$setsolve() they are "internal" functions to be called
##              by cacheSolve()
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setsolve <- function(solve) m <<- solve
	getsolve <- function() m
	list (set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## Return a matrix that is the inverse of x, but first check if the calculation
## has alredy been done
## 
## Use:
## cacheSolve(x) notice x object is the one created with the previous function
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		
		## 1. We check if there is data in x object
		##
		m = x$getsolve()
		if (!is.null(m)) {
			## 2. If it is data, we'll load it from the x object
			##
			message("getting cache data"			
			return(m)
		}
		## 3. If there isn't we'll get the initial object created with
		##    obj = makeCacheMatrix(mat)
		data <- x$get()
		m <- solve(data, ...)
		## 4. After the solve function is done, the results are stored in
		##    the x object, and finally return the result
		x$setsolve(m)
		m
}
