## Kyle's Inverse Matrix Caching Functions
## I don't usually comment this much, but I just wanted to show that
## I understand how these functions work.

# makeCacheMatrix below takes an input x, the matrix to be solved, 
# and returns a list of functions that either fetch or set objects.

makeCacheMatrix <- function(x = matrix()) {
	# We initialize the value of matrix_inverse first to be NULL
	# to indicate that we haven't cached anything.
	matrix_inverse <- NULL
        
		# This function is only used if you want to change the original
		# matrix to another matrix. Assuming a matrix_inverse has already
		# been calculated, it changes the value back to NULL as a way to
		# reset, so to speak.
        set_matrix <- function(y) {
                x <<- y
                matrix_inverse <<- NULL
        }

    # The fetch_matrix function simply returns the matrix to be solved.    
	fetch_matrix <- function() x

	# The set_inverse matrix function changes the value of matrix_inverse from 
	# NULL to the solved inverse.
	set_inverse <- function(inverse) matrix_inverse <<- inverse

	# The fetch_inverse function simply returns the value of matrix_inverse.
	fetch_inverse <- function() matrix_inverse

	# makeCacheMatrix returns a list of the above functions
	# which return their respective values.
	list(set_matrix = set_matrix, fetch_matrix = fetch_matrix, set_inverse = set_inverse, fetch_inverse = fetch_inverse)
}


# cacheSolve below takes an input x, the result of calling makeCacheMatrix
# and returns the solved inverse of the matrix argument in makeCacheMatrix.

cacheSolve <- function(x, ...) {
	# matrix_inverse calls the fetch_inverse() function from above which
	# returns the value of matrix_inverse. This is either NULL, which means
	# the inverse has not been computed yet, or a matrix, which is the 
	# actual inverse that has been cached.
	matrix_inverse <- x$fetch_inverse()

	# This if statement checks to see if the inverse has not been
	# computed yet, NULL, or has been computed. If it is not NULL, which
	# means it has been computed already, it will say the message and return
	# the cached matrix.
	if(!is.null(matrix_inverse)) {
		message("No need to calculate again, fetching cached data")
		return(matrix_inverse)
	}

	# Else, meaning matrix_inverse is NULL, it will proceed to
	# compute the inverse of the matrix. It will first fetch the actual
	# matrix and put it into this variable called "matrix"
	matrix <- x$fetch_matrix()

	# Then solve the inverse of the matrix
	matrix_inverse <- solve(matrix, ...)

	# Then put that value into matrix_inverse by calling the
	# set_inverse() function.
	x$set_inverse(matrix_inverse)

	# It will then return the solved matrix_inverse.
	matrix_inverse
}