# This set of functions work together to create an matrix object 
# capable of caching it's inverse for future use and so skiping
# recalculation process for already calculated inverses
#
# The functions for CacheMatrix generation and system solve are followings:
#   * makeCacheMatrix  - Creates a CacheMatrix
#   * cacheSolve       - Returns matrix system solution
#
# The CacheMatrix object type has the following set of methods
#   set( matrix() )    - stores a new matrix
#   get()              - retrieves current matrix
#   setinv( matrix() ) - stores a new inverse matrix
#   getinv()           - retrieves the inverse matrix
#
# examples
# >  X <- makeCacheMatrix( matrix(c(1,2,3,1), 2, 2) )
# >  X$get()
#      [,1] [,2]
# [1,]    1    3
# [2,]    2    1
#
# >  Xinv <- cacheSolve( X )
# >  Xinv
#      [,1] [,2]
# [1,] -0.2  0.6
# [2,]  0.4 -0.2
#   
# A second call to cacheSolve will retrieve previously cached inverse
# > Xinv <- cacheSolve( X )
# getting cached inverse
# >  Xinv
#      [,1] [,2]
# [1,] -0.2  0.6
# [2,]  0.4 -0.2
#


#' roxygen documentation 
#' \link{http://cran.r-project.org/web/packages/roxygen2}
#'
#' This set of functions work together to create an matrix object 
#' capable of caching it's inverse for future use and so skiping
#' recalculation process for already calculated inverses
#'
#' The functions for CacheMatrix generation and system solve are followings:
#' \describe {
#'  \item {\code{\link{makeCacheMatrix}}} {Create a CacheMatrix}
#'  \item {\code{\link{cacheSolve}}}      {Solves matrix system}
#' }
#'
#' The CacheMatrix object type has the following set of methods
#' \itemize {
#'   \code{\link{set()}}    - stores a new matrix
#'   \code{\link{get()}}    - retrieves current matrix
#'   \code{\link{setinv()}} - stores a new inverse matrix
#'   \code{\link{getinv()}} - retrieves the inverse matrix
#' }
#'
#' @examples
#' >  \code{X <- \link{makeCacheMatrix}( matrix(c(1,2,3,1), 2, 2) )}
#' >  \code{X$\link{get}()}
#'      [,1] [,2]
#' [1,]    1    3
#' [2,]    2    1
#'
#' >  Xinv <- \code{\link{cacheSolve}( X )}
#' >  Xinv
#'      [,1] [,2]
#' [1,] -0.2  0.6
#' [2,]  0.4 -0.2
#'   
#'  A second call to \link{cacheSolve} will retrieve previously cached inverse
#' > \code{Xinv <- \link{cacheSolve}( X )}
#' getting cached inverse
#' >  Xinv
#'      [,1] [,2]
#' [1,] -0.2  0.6
#' [2,]  0.4 -0.2
#'
#' @references \url{https://github.com/rdpeng/ProgrammingAssignment2/blob/master/README.md}
#' @author Paulo Santiago Ribeiro 
#'
#' @name cachematrix 
NULL

#' Create an object containing a matrix and which may cache 
#' its inverse calculated result for further references
#' 
#' @param \code{x} Numeric matrix - default empty \code{\link{matrix()}}
#' @return an matrix object which may cache its inverse
#' @examples
#'   X <- makeCacheMatrix( matrix(c(1,2,3,1), 2, 2) )
#'   Y <- makeCacheMatrix( matrix(c(1,2,3,3,2,1,3,1,2),3,3 ) )
#'
#' \dontrun {
#'   A <- makeCacheMatrix( matrix(c("a","b","c","d"), 2, 2) )
#' }
makeCacheMatrix <- function(x = matrix()) {
        cached <- NULL               # initialize as invalid object 

        #' CacheMatrix setter
        #' Stores a new matrix object and clears any previous cached objects
        #'  
        #' @param y Numeric \link{matrix} to be stored
        #' @examples
        #'   CacheMatrix$set( matrix( c(1,2,3,1),2,2) )  
        set <- function(y) {
                x      <<- y         # set new matrix object
                cached <<- NULL      # clear cached inverse
        }

        #' CacheMatrix getter
        #' Retrieves the underlying matrix object
        #'
        #' @return stored \link{matrix}
        #' @examples
        #'   Var <- CacheMatrix$get()
        get <- function() x

        #' CacheMatrix inverse setter  
        #' Allows the calculated inverse to be stored for future reference
        #'
        #' @param inv Numeric \link{matrix}
        #' @examples
        #'   CacheMatrix$setinv( solve( CacheMatrix$get() ) )
        setinv <- function(inv) cached <<- inv 

        #' CacheMatrix inverse getter 
        #' Retrieves previously cached inverse matrix
        #' NULL means there's no cached inverse
        #'
        #' @return cached inverse matrix or NULL
        #' @examples
        #'   CacheMatrixInv <- CacheMatrix$getinv()
        getinv <- function() cached 

        list(set = set, get = get,   ## returns special matrix object 
             setinv = setinv,
             getinv = getinv)
}


#' Provides the inverse of matrix which solves the system 
#' y = A %*% x    so that     x = Ainv %*% y
#' This function first looks for a cached result so that it may
#' skip the calculation process already done
#'
#' @param x   cacheable matrix
#' @param ... parameters to be passed to the \link{solve} function
#' @inheritParams solve
#' @return    inverse of matrix x
#' @seealso \code{\link{solve}}
#' @section Warning:
#'   its assumed that the matrix supplied is always invertible.
#'
cacheSolve <- function(x, ...) {

        # try to obtain an previously calculated inverse
        inv <- x$getinv()      
        if( !is.null(inv) ) {   
                # inverse has been calculated already return it
                message( "getting cached inverse" ) 
                return(inv)
        }

        # inverse matrix was not calculated yet, so calculate it
        inv <- solve( x$get(), ... )

        # ... and store on cache for future retrieval
        x$setinv(inv) 

        # return inverse
        inv
}
