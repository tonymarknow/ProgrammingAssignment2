makechacematrix<-function(x=matrix()){
  man<-NULL
  set<-function(y){
    x<<-y
    man<<-NULL
  }
  get<-function(){x}
  setinv<-function(inverse)(man<<-inverse)
  getinv<-function()(man)
  list(set=set,get=get,setinv=setinv,getinv=getinv)
  
}
chacesolve<-function(x,...){
  man<-x$getinv()
  if(!is.null(man)){
    message("obtaining cached data")
    return(man)
  }
  utd<-x$get()
  man<-solve(utd,...)
  x$setinv(man)
  man}
