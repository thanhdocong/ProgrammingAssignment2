## Inverse of a Matrix
## caching is used to inverse of a matrix rather than compute it repeatedly
## cache the inverse of matrix if it is not changed
makeCacheMatrix <- function(x = matrix()) {
m<-NULL
set<-function(y){
x<<-y
m<<-NULL
}
get<-function() x
setmatrix<-function(solve) m <<- solve
getmatrix<-function() m
list(set=set, get=get,
setmatrix=setmatrix,
getmatrix=getmatrix)
}
## computes the inverse of the matrix
cacheSolve <- function(x, ...) {
m<-x$getmatrix()
if(!is.null(m)){
print("getting cached data")
return(m)
}
matrix<-x$get()
if(dim(matrix)[1] != dim(matrix)[2]){
print("Passed in matrix is not square matrix. To compute inverse matrix need to be square matrixe. ")
NA
}else {
m<-solve(matrix, ...)
x$setmatrix(m)
m
}
}
