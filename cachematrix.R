
## makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(mtx_input = matrix()){
  
  inverse<-NULL
  
  ##function for setting up the matrix
  set_matrix<-function(mt){
    mtx_input<<-mt
    inverse<<-NULL ##reset inverse result for new matrix
  }
  ##function for returning the matrix
  get_matrix<-function(){
    return(mtx_input)
  }
  ##function for setting up inverse for the matrix
  set_inverse<-function(inv){
    inverse<<-inv
  }
  ##function for returning the inverse of the matrix
  get_inverse<-function(){
    return(inverse)
  }
  
  ##return result of the makeCacheMatrix
  return(list(set_matrix=set_matrix, get_matrix=get_matrix,
              set_inverse=set_inverse, get_inverse=get_inverse
  )
  )
  ##end of makeCacheMatrix
}

##cacheSolve  computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed),
##then cacheSolve should retrieve the inverse from the cache.

cacheSolve<-function(mtx_input,...){
  ##retrieve inverse through get_inverse() function
  inv<-mtx_input$get_inverse()
  
  
  ##if: inv is not null-->return inverse + break function
  if(!is.null(inv)){
    message("Getting cached inverse")
    return(inv)
  }
  ##else: inv is null--> performe calculation on
  ##      matrix returned by get_matrix() then
  ##      return the result
  else{
    inv=solve(x$get_matrix())
    x$set_inverse(inv)
    return(inv)
  }
  ##end of cacheSolve
}