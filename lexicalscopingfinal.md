makeCacheMatrix <- function(x = matrix()){
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL #This allows to ou function to access both parent and child enviorment
    }
    get <- function() {x} #Get the value of the matrix 
    setInverse <- function(inverse) {inv <<- inverse} #Setting the value of the inverse matrix
    getInverse <- function() {inv}  #Getting the value of the inverse matrix
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...){
    inv <- x$getInverse() #Returns a matrix that is the inverse of x and assigns to in variable
    if(!is.null(inv)){
        message("getting cached data")
        return(inv) #Verifying if the inverse of the matrix is computed
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv) #Setting the value of the inverse matrix in the cache
    inv
}