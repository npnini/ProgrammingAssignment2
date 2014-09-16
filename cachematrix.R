## makeCacheMatrix - this function recieves a matrix and supports the following methods: set/set the matrix values, get/set the inverse matrix
## cacheSolve - this function recieves a matrix object. If a cached inverse matrix already exists, it is returned. 
## if a cached inverted matrix does not exist, it is computed, cached, then returned.
##
## makeCacheMatrix - this function: can store a raw matrix and its inverted matrix; exposes get/set methods of the raw matrix, get/set methods of the inverted matrix
##
makeCacheMatrix <- function(raw_mat = matrix()) {
inv_mat <- NULL
set <- function(y) {
raw_mat <<- y
inv_mat <<- NULL
}
get <- function() raw_mat
setinv <- function(inv) inv_mat<<- inv
getinv <- function() inv_mat
list(set = set, get = get,
setinv = setinv,
getinv = getinv)
}
##
## cacheSolve - for an input matrix, this function: 
## 	returns an inverted matrix if it is already cached; 
##	computes, then caches, then returns the inverted matrix if it is not yet cached
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
m <- x$getinv()
if(!is.null(m)) {
message("getting cached data")
return(m)
}
raw_mat <- x$get()
m <- solve(raw_mat) %*% raw_mat
x$setinv(m)
return(m)
}
