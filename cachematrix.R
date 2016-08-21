

#makeCacheMatrix creates a list that will be used by the other function cacheSolve.
#within the list created we define the value of the matrix (set)
# we get the value of the matrix if required (get)
#we set the inverse of the matrix (setinverse) once done by cacheSolv
# we get the inverse of the matrix if required (geinverse)

makeCacheMatrix <- function(x = matrix()) {
        n <- NULL
        set <- function(y){
                x <<- y
                n <<- NULL
        }
        get <- function() x
        setinverse <- function(inver) n <<- inver
        getinverse <- function() n
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


#CacheSolve take the matrix created by makeCacheMatrix and returns it's inverse.
#The function checks if the inverse has already been calculated and it brings back the stored value
#If it has been calculated it uses the solve function to calculate it and store it.

cacheSolve <- function(x, ...) { 
        n <- x$getinverse()
        if(!is.null(n)){
                message("Retrieving cached data.")
                return(n)
        }
        data <- x$get()
        n <- solve(data)
        x$setinverse(n)
        n
}
