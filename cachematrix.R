## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL # Initialising the inverse to be NULL since this stores the inverse of the input matrix
    set<-function(a)
    { x<<-a
      inv<<-NULL
    }   # Storing the input matrix
    get<-function() x # Inputting the matrix
    set_inverse<-function(inverse) inv<<-inverse   # Caluclates the inverse matrix
    get_inverse<-function() inv # Invokes the stored inverse matrix
    list(set=set,get=get,set_inverse=set_inverse,get_inverse=get_inverse)
}

cacheSolve <- function(x, ...) {
  inv<-x$get_inverse() 
  if(!is.null(inv)) # checks for already calculated inverse of the same matrix
  {  print("Present in cache. Getting the data...")
     return(inv)
    
  }  
  # If already calculated inverse is not present , 
  # it gets the new matrix from the user, calculates it and stores it.
  mat<-x$get()
  inv<-solve(mat,...)
  x$set_inverse(inv)
  inv
}
