##This is second part of assignment
cacheSolve <- function(x, ...) 
{
if(require("corpcor")){
	print("The loading of corpcor is done correctly")
	} else {
		print("Corpcor not found, so trying to install corpcor...")
		install.packages("corpcor")
		if(require(corpcor)){
			print("Corpcor is installed and loaded")
			} else {
			stop("Corpcor could not be installed. Please try later")
			}
		}
inverse <- x$getinverse()
if(!is.null(inverse)){
	message("The matrix is in memory")
	return(inverse)
	}
message("The matrix inverse is not in memory so the inverse is gonna be computed if possible")
data <- x$get()
inverse <- pseudoinverse(data, ...)
x$setinverse(inverse)
inverse
}


#Below are few experiments tried to check if it works
#lets check for square matrix first
x <- matrix(rpois(16, 2), nrow = 4)
mcmx <- makeCacheMatrix(x)
mcmx$get()
cacheSolve(mcmx)
cacheSolve(mcmx)
invx <- cacheSolve(mcmx)

#lets check with rectangular matrix where row > col
y <- matrix(rpois(15, 2), nrow = 5, ncol = 3)
mcmy <- makeCacheMatrix(y)
mcmy$get()
cacheSolve(mcmy)
cacheSolve(mcmy)
invy <- cacheSolve(mcmy)

#lets check with rectangular matrix where row < col
z <- matrix(rpois(15, 2), nrow = 3, ncol = 5)
mcmz <- makeCacheMatrix(z)
mcmz$get()
cacheSolve(mcmz)
cacheSolve(mcmz)
invz <- cacheSolve(mcmz)

#Below we check whether the multiplication of matrix and its inverse gives identity or not
invx %*% x 
x %*% invx
invy %*% y
y %*% invy
invz %*% z
z %*% invz 
