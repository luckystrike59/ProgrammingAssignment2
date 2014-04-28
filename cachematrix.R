## makeCacheMatrix: Takes in a matrix and stores its inverse. The matrix that is passed in is converted to a vector and
##                  stored in a global variable called lstMatrix. At the same time, the inverse of the matrix is calculated
##                  and converted to a vector which is stored in a global variable called lstInverse. 
##                  The index of the matrix stored in lstMatrix should match the index of the inverse of that matrix in
##                  lstInverse. The lstMatrix will function as a lookup to determine if the matrix passed into this function
##                  has its inverse stored in lstInverse. If so, then there is no need to recalculate the inverse and it is 
##                  retrieved form lstInverse and passed back to the calling program.

makeCacheMatrix <- function(x = matrix()) {
    #initialize variables
    found <- FALSE
    location <- 0
    returnValue <- NULL

    #Initialize the global vectors. The statements within this if statement are executed if this function is being called
    #for the first time
    if(!exists("lstMatrix")){
        #Make sure that the inverse can successfully be derived from the matrix
        valid <- tryCatch(lstInverse <<- list(c(solve(x))),error = function(c) FALSE)
        
        #Return an error message
        if(identical(valid,FALSE)){
            returnValue <- c("error creating inverse")
        }
        else{
            lstMatrix <<- list(c(x))
        }
        returnValue <- lstInverse[[1]]
    }
    #Look for an existing inverse based on matrix that is passed in
    else{
        for(i in 1:length(lstMatrix)){
            #If a match is found then lookup up the inverse and return to the calling program
            if(identical(lstMatrix[[i]],c(x))){
                location <- i
                found <- TRUE
                break
            }            
        }
        
        if(identical(found,TRUE)){
            returnValue <- lstInverse[[location]] 
        }
        #New matrix so add the matrix and inverse matrix to the global variables
        else{     
            #Make sure that the inverse can successfully be derived from the matrix
            valid <- tryCatch(lstInverse <<- append(lstInverse,list(c(solve(x)))),error = function(c) FALSE)
            if(identical(valid,FALSE)){
                returnValue <- c("error creating inverse")
            }
            else{
                lstMatrix <<- append(lstMatrix,list(c(x)))
                returnValue <- lstInverse[[length(lstInverse)]]
            }
            #return the newly created inverse
        }
    }
    returnValue
}


## cacheSolve:  Makes a call to makeCacheMatrix to retrieve the inverse of a matrix. This function will only calculate
##              the inverse if it hasn't been calculated before.
cacheSolve <- function(x, ...) {
    #Store returned vector
    invMatrix <- makeCacheMatrix(x)
    
    #calculate the dimensions of the matrix by getting th square root of the length of the returned vector
    dim <- sqrt(length(invMatrix))

    #cast the vector to a matrix
    matrix(invMatrix,dim,dim)
#     }

}
