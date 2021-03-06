## Put comments here that give an overall description of what your
## functions do

# This function creates a special "matrix" object that can cache its inverse.
# Author: Mark Belkin
# 10/24/2015

makeCacheMatrix <- function(x = matrix()) {
      
	# Initialization
	#
	inv_mtr <- NULL

	# set matrix function
	#
      set <- function(y=NULL) {
		if (!is.null(y)) {
      		mtr <<- y
			inv_mtr <<- NULL
		}
		else
			message ("Please specify matrix to be set")
      }

	# get matrix function
	#
      get <- function() {
		if (!is.null(mtr))
      		mtr
		else
			message ("Matrix has not been set")
	}

	# set inversed matrix function
	#
      set_inv <- function(org_mtr=NULL) {
		if (!is.null(org_mtr)) {
			# If determinant = 0 , matrix is not inversible
			if (det(org_mtr)==0) {
		    		message("Matrix is singular and therefore is not revesible")
                		return(org_mtr)
        		}
			else	{
				message ("Caching inverted matrix")
				inv_mtr <<- solve(org_mtr)
			}
		}
		else
			message ("Please specify matrix to be inverted and cached")
      }

	# get inversed matrix function
	#
	get_inv <- function() {
		if(!is.null(inv_mtr)) {
        		inv_mtr
        	} else
			message("Inversed matrix has not been cached")
	
	}      
  	list(set = set, get = get,
             set_inv = set_inv,
             get_inv = get_inv)

}


# The function cacheSolve checks whether inversion if the matrix,
# used as an input has been cached
#      If yes, then the function returns cached inverted matrix
#      If no,  then the function inverts the input matrix and returns results
#

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
            # Reading inverted matrix
    #
    inv <- x$get_inv()
    if(!is.null(inv)) {
        message("Getting cached data.")
        return(inv)
    }
    data <- x$get()
    x$set_inv(data)		
    inv <- x$get_inv()
    inv

}
