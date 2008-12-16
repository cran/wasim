`k_rel` <-
function(x,y){
    rel <- k_hyd(x) / k_hyd(y)
    if(max(rel,na.rm=TRUE)>(1/min(rel,na.rm=TRUE))){
        toRet<-max(rel,na.rm=TRUE)
    } else {
        toRet<-min(rel,na.rm=TRUE)
    }
    return(toRet)
}

