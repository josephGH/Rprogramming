## INVERSE MATRIX
## ==============

## makeCacheMatrix creates a special vector to define the matrix and functions

makeCacheMatrix <- function(x = matrix(),index=dim(x)) {
		## Define "m" as null matrix
		m<- NULL
		## Take the dimensions of the matrix defined as parameter
		p<-index[1]
		q<-index[2]
		set<-function(y) {
			x <<- y
			m <<- NULL
			p <<- index[1]
			q <<- index[2]
		}
		get<-function() x
		## This conditional will calculate the special vector
		if (p==q){
		setsolve <- function(solve) m <<- solve
		getsolve <- function() m
		list(set=set,get=get,setsolve=setsolve,getsolve=getsolve)
		}
		else
		{ list(set=set,get=get,setsolve=setsolve,getsolve=getsolve)}

}


## CacheSolve will calculate the inverse matrix

cacheSolve <- function(x, ...) {
		m <- x$getsolve()
		if(!is.null(m)) {
				message("This Matrix not has INVERSE........!!!")
				message("This Matrix should be squared........!!!")
				message("Getting inverse matrix from cache")
				return(m)
		}
		data<- x$get()
		m <- solve(data, ...)
		x$setsolve(m)
		m				
        
}
