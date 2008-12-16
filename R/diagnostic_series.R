`diagnostic_series` <-
function(
measured,
modelled,
duration,
integral_correction=FALSE, use_qualV=FALSE
){
t.pos<-1:(NROW(modelled)-duration)
dim(t.pos)<-c((NROW(modelled)-duration),1)
r.diag<-do.call("rbind", lapply(t.pos, FUN=diagnostic_window, duration=duration, measured=measured, modelled=modelled,use_qualV=use_qualV))


#multiplicative correction of integrated flow
#time series need the same number of NA
if(integral_correction){
    correctA<-measured
    correctA[is.na(modelled)]<-NA
    correctB<-modelled
    correctB[is.na(correctA)]<-NA

    correct.integral<-sum(correctB,na.rm=TRUE)/sum(correctA,na.rm=TRUE)
    r.diag$I<-r.diag$I*correct.integral
}

t.diff<-measured-modelled
diff.ecdf <- ecdf(t.diff)
toAppend<-data.frame(matrix(nrow=duration,ncol=NCOL(r.diag)) )
names(toAppend) <- names(r.diag)
r.diag<-rbind(toAppend,r.diag)

r.diag$rel_diff=c(diff(measured) / diff(modelled), NA)
krel=k_hyd(measured) / k_hyd(modelled)
r.diag$EQ=diff.ecdf(t.diff)
return(r.diag)

}

