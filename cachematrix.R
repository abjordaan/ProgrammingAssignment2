# makeCahceMatrix  -> create special matrix
makeCacheMatrix <- function(x = matrix()) {
        
        #  set the matrix
        #  get the matrix
        #  set the inverse of the matrix
        #  get the inverse of the matrix
        
        # set to NULL
        inv <- NULL
        
        # set the matrix, noo inverse
        set_matrix <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        # get the matrix
        get_matrix <- function() x
        
        # set the inverse of the matrix
        set_inverse <- function(inverse) inv <<- inverse
        
        # get the matrix
        get_inverse <- function() inv
        
        # pass all to a list
        list(set_matrix=set_matrix, 
             get_matrix=get_matrix,
             set_inverse=set_inverse,
             get_inverse=get_inverse)
}


# cacheSolve to calculate the inverse of the matrix. Also refer to cache if the matrix exists or not.
cacheSolve<- function(x, ...) {
        
        # inverse of the matrix
        inv <- x$get_inverse()
        
        # if matrix exist in the cache
        if(!is.null(inv)) {
                # display message if the inverse si retrived from cache
                message("getting cached data.")
                return(inv)
        }
        
        # not in cache, get hte matrix itself, the firts int of the matrix
        data <- x$get_matrix()
        
        # inverse of the matrix
        inv <- solve(data)
        
        # set the inverse
        x$set_inverse(inv)
        inv
}