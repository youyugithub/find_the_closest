# find_the_closest

```
find_the_closest<-function(x,mask=rep(T,length(x))){
  idx<-mask&(x>=0)
  if(any(idx)){
    xmin<-min(x[mask&(x>=0)],na.rm=TRUE)
    return(x==xmin)
  }else{
    return(rep(F,length(x)))
  }
}
```
