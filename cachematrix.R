## github- poshitha999


## This function creates a special "matrix" object that can cache its inverse

 makeCacheMatrix<-function(x=matrix()){   #define the argument with the default mode of matrix.
    inv<-NULL                             ## initialize inv as NULL; will hold value of matrix inverse   
    set<-function(y){                      #define the set function to assign new       
      x<<-y                               #value of matrix in parent environment
      inv<<-NULL                          ## if there is a new matrix, reset inv to NUL
      
    }
    
    get<-function(){                       ## define the get function which returns the value of matrix argument 
      x
    }
    
    setInverse<-function(inverse){        ## assigns value of inv in parent environment  
      inv<<-inverse                       ## gets the value of inv where called
    }
    
    getInverse<-function(){
      inv
    }
    
    list(set=set,get=get,setInverse=setInverse, getInverse=getInverse)
  }
  
 ## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
 ## If the inverse has already been calculated (and the matrix has not changed),then cacheSolve will retrieve the inverse from the cache

 
  cacheSolve<-function(x,...){
    inv<- x$getInverse()                                    ## Return a matrix that is the inverse of 'x
    if(!is.null(inv)){
      message("getting cached data")
      return(inv)
    }
    
    mat<-x$get()
    inv<-solve(mat,...)
    x$setInverse(inv)
    inv
    
  }
  
