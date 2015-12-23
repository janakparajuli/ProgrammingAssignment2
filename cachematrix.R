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
