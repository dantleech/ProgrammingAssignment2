## These functions allow the result of `solve()` method to be associated with
## an object which represents a "cached" matrix.

## The `makeCacheMatrix()` function returns a list of functions allowing the
## matrix value to be gotten and provides methods to allow caching.
## `setinv()` sets the `inv` variable in the parent scope. `getinv()`
## returns this same variable.

makeCacheMatrix <- function(m = matrix()) {
    inv <- NULL
    get <- function () m
    setinv <- function(inv) {
        inv <<- inv
    }
    getinv <- function () inv
    list(get = get, setinv = setinv, getinv = getinv)
}


# The `cacheSolve()` function determines if the inverse has already been set
# on the given "cached matrix" (as returned from the function above), it it
# has already been set (it is not NULL) then it is returned, otherwise the
# value is computed and `setinv()` is called to cache the value for subsequent
# calls.

cacheSolve <- function(cm) {
    inv = cm$getinv()

    if (!is.null(inv)) {
        message("Getting inversed matrix")
        return(inv)
    }

    m <- cm$get()
    cm$setinv(solve(m))
    cm$getinv()
}
