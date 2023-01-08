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

```
find_the_closest<-function(x,target,mask=rep(F,length(x))){
  if(any(!mask)){
    alldiff<-x-target
    unmaskdiff<-alldiff[!mask]
    mindiff<-unmaskdiff[which.min(abs(unmaskdiff))]
    idx<-which(alldiff==mindiff&!mask)[1]
    return(1:length(x)==idx)
  }else{
    return(rep(F,length(x)))
  }
}

is_closest<-function(x,target,filter=rep(T,length(x)),tie=seq(1,length(x))){
  if(any(filter)){
    allabsdiff<-abs(x-target)
    filterabsdiff<-allabsdiff[filter]
    minabsdiff<-min(allabsdiff)
    closest<-allabsdiff==minabsdiff&filter
    idx<-which(closest&tie==min(tie[closest]))[1]
    return(1:length(x)==idx)
  }else{
    return(rep(F,length(x)))
  }
}
```
