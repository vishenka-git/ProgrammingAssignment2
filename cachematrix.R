## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { # x is a default empty matrix
        
        matrix_inverse <- NULL # set a empty variable for the result of inverse
        set <- function(y) {
                x <<- y # y deliver the new matrix input
                matrix_inverse <<- NULL #the old result will be changed to null
        }
        get <- function() x #get the input matrix
        setinverse <- function(inverse) matrix_inverse <<- inverse #deliver the actual inverse result to variable "matrix_inverse"
        getinverse <- function() matrix_inverse #get the result from variable "matrix_inverse"
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse) #set the list for various functions
        
}


## Write a short comment describing this function
##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getinverse() # x will present the function "makeCacheMatrix()"
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        } #Using the getinverse() from makeCacheMatrix function to get the result, if no change, just return the result.
        data <- x$get() #When it find out the m is null with new input, it will get the new matrix input with get() function from makeCacheMatrix()
        m <- solve(data,...) #calculate the new result with using solve()
        x$setinverse(m) #set the new result to variable "matrix_inverse" in makeCacheMatrix()
        m #return the new result
}

matrix<-matrix(sample(1:64,64),8,8)
mymatrix<-makeCacheMatrix(matrix)
cacheSolve(mymatrix)