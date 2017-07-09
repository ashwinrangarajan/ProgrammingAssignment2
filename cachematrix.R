## Writing the makecachematrix function which will create the matrix, set and get the value of a matrix; set and get the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {

		#initializing the cached value
		cached <- NULL
		
		#setting the working environment		
		set	<- function(y){
				x	<<-	y
				cached	<<-	NULL
		}
		
		#value of matrix is being fetched
		get <- function() x
		
		setinvmatrix <- function(inverse) cached <<- inverse
		getinvmatrix <- function() cached
		list(set = set,
			 get = get, 
			 setinvmatrix = setinvmatrix, 
			 getinvmatrix = getinvmatrix)
}


## Below function calculates the inverse of the matrix created by the above function.
## It is also designed to check if the inverse is already calculated or not.

cacheSolve <- function(x, ...) {
        #get inverse of stored in cache
        cached <- x$getinvmatrix
        
        #return inverted matrix from cache if exists
        if(!is.null(cached)) {
        		message("getting cached inverse data")
        		return(cached)
        }
        
        #create matrix since it doesn't exist
        matrix <- x$get()
        
        cached <- solve(matrix,...)
        
        x$setinvmatrix(cached)
        
        cached
}
