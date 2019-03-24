# The following code provides an output of a Matrix Inverse from the initial 
# matrix given by the user through the function makeCacheMatrix. Through the 
# makeCacheMatrix function, the value of the matrix is set and get, in order to 
# set and get the inverse of the matrix inputted. 

# Another operator can be seen along the line of codes as well, denoted by '<<-',
# which is used to assign variables in the parent environment. This type of
# assignment operator is different from '<-' wherein the latter is assigned to 
# a variable within the same environment. 

# The line below is the function named makeCacheMatrix which is called at the start
# of running the program code. More importantly, it dictates that the variable inside
# the function should be a matrix, which will be called as 'x' in the succeeding lines.
makeCacheMatrix <- function(x = matrix()){
    
    # MatrixInverse is initially set as NULL 
    MatrixInverse <- NULL
    
    # setMatrix sets the value of the inputted matrix.
    setMatrix <- function(y){
        x <<- y
        MatrixInverse <<- NULL
    }
    
    # getMatrix gets the value of the matrix via 'x'
    getMatrix <- function() x
    
    # setMatrixInverse sets the value of the invertible matrix through the 
    # function variable 'inverse' 
    setMatrixInverse <- function(inverse) MatrixInverse <<- inverse
    
    # getMatrixInverse gets the value of the invertible matrix through the 
    # function MatrixInverse as the variable 'inverse' was stored to it in the
    # former code line
    getMatrixInverse <- function() MatrixInverse
    
    list(setMatrix = setMatrix, getMatrix = getMatrix, 
         setMatrixInverse = setMatrixInverse, getMatrixInverse = getMatrixInverse)
}


# The next line of codes are executed by calling the 'cacheSolve' function that 
# takes the output of the makeCacheMatrix as its input. It further evaluates whether
# the 'getMatrixInverse' has a value or not. If the matrix inverse does not have
# a value or is empty, the original matrix is obtained to be evaluated as the 
# invertible matrix through the 'solve' function. Otherwise, it returns a message
# "Getting Cached Data" before returning the invertible matrix. 
cacheSolve <- function(x, ...){
    MatrixInverse <- x$getMatrixInverse()
    
    # If the inverse matrix is not null, the message "Getting Cached Data" is
    # displayed before returning the invertible matrix.
    if(!is.null(MatrixInverse)){
        message("Getting Cached Data")
        return(MatrixInverse)
    }
    
    # If the inverse matrix is null or does not have a value, the original matrix
    # is called to get the original matrix.
    data <- x$getMatrix()
    
    # The 'solve' function is used to inverse the matrix.
    MatrixInverse <- solve(data, ...)
    
    # The invertible matrix is set from MatrixInverse.
    x$setMatrixInverse(MatrixInverse)
    
    # The invertible matrix is returned. 
    MatrixInverse
}