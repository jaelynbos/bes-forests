NormList<-sapply((1:1000),rnorm)
SDs<-unlist(lapply(NormList,sd))
plot(1:1000,SDs)

#Shows convergence to 1. Now let's do something a little different:

#Let's make a sequence counting to 1000 by tens

DumSeq<-seq(10,1000,by=10)
DumSeqL<-list(DumSeq)

#Make 10 of each unique value
DumSeqL<-lapply(DumSeq,rep,10)

#Make rnorm set for each value

DumFun<-function(x){
	return(sapply(x,rnorm))
	}

NormList2<-lapply(DumSeqL,DumFun)

#Take the sd of each
DumFun1<-function(x){
	return(apply(x,2,sd))
	}

SDs2<-lapply(NormList2,DumFun1)

#Then take the sd of the sds for each

DoubleSD<-unlist(lapply(SDs2,sd))

#And plot

plot(1:100,DoubleSD)