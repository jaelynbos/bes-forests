################################################################################
#Species richness and Shannon diversity
#Boxplots and nCPAs
#Jaelyn Bos
################################################################################

################################################################################
#Prior to running this script, run "Create OS community data.R" 
#Required packages: vegan
################################################################################

library(vegan)

occurance<-function(x){
	if (x>0){
	return(1)
	}
	else{
	return(0)
	}
}

##############################################################################
#Overstory richness nCPA
##############################################################################
OsTaxaOccurance<-apply(OSTaxaMatrix1,c(1,2),occurance)

#Calculate overstory spp richness
OsRichness<-rowSums(OsTaxaOccurance)

#Calculate minimum distance to edge or valley
OsMins<-pmin(OsAll$dist2edge,OsAll$dist2val)

#Calculate Poisson change point for species richness
RichOsCpa<-cp.pois(OsMins,OsRichness,data=as.data.frame(cbind(OsMins,OsRichness)))

#Plot richness points along min distance to edge gradient
plot(RichOSCPA$xx,RichOSCPA$yy,xlab="Min distance to edge or valley",ylab="Overstory species richness",main="Poisson change point overstory species richness")

#Add rectangle with 10th and 90th percentile change points
rect(xleft=quantile(RichOSCPA$bootrep[,1],0.1),xright=quantile(RichOSCPA$bootrep[,1],0.9),ybottom=0,ytop=13,density=50)

#Add line with change point
abline(v=RichOSCPA$all.out[1],col="red",lwd=2)

#Print quantiles
quantile(RichOSCPA$bootrep[,1],0.1)
quantile(RichOSCPA$bootrep[,1],0.9)
RichOSCPA$all.out[1]

##############################################################################
#Overstory Shannon diversity index nCPA
##############################################################################

#Calculate Shannon diversity index for overstory spp
OsShannon<-diversity(OSTaxaMatrix1,index="shannon")

#Calculate Poisson change point for Shannon diversity index
ShannonOsCpa<-cp.pois(OsMins,OsShannon,data=as.data.frame(cbind(OsMins,OsShannon)))

#Plot Shannon points along min distance to edge gradient
plot(ShannonOSCPA$xx,ShannonOSCPA$yy,xlab="Min distance to edge or valley",ylab="Overstory Shannon diversity",main="Poisson change point overstory Shannon diversity")

#Add rectangle with 10th and 90th percentile points
rect(xleft=quantile(ShannonOSCPA$bootrep[,1],0.1),xright=quantile(ShannonOSCPA$bootrep[,1],0.9),ybottom=0,ytop=13,density=50)

#Add line with change point
abline(v=ShannonOSCPA$all.out[1],col="red",lwd=2)

#Print quantiles
quantile(ShannonOSCPA$bootrep[,1],0.1)
quantile(ShannonOSCPA$bootrep[,1],0.9)
ShannonOSCPA$all.out[1]

##############################################################################
#Overstory richness boxplot
##############################################################################
RichMspa<-as.factor(OsAll$mspa)
levels(RichMspa)<-c("other","connected canopy","edge","other","core","connected canopy","edge","connected canopy","edge","other","edge","other")

boxplot(OsShannon~RichMspa,notch=T,xlab="MSPA class",main="Overstory Shannon diversity by MSPA class",ylab="Shannon diversity index")

##############################################################################
#Overstory Shannon boxplot
##############################################################################

boxplot(OsRichness~RichMspa,notch=T,xlab="MSPA class",main="Overstory species richness by MSPA class",ylab="Species richness")

##############################################################################
#Overstory stem density
##############################################################################

#Calculate Gaussian change point for stem density
StemOsCpa<-cp.gauss(OsMins,rowSums(OSTaxaMatrix1),data=as.data.frame(cbind(OsMins,rowSums(OSTaxaMatrix1)))

#Plot stem density on minimum distance to edge gradient
plot(StemOSCPA$xx,StemOSCPA$yy,xlab="Min distance to edge or valley",ylab="Overstory stem  density",main="Gaussian change point overstory stem density")

#Add rectancle with 10th and 90th percentile of nCPA
rect(xleft=quantile(StemOSCPA$bootrep[,1],0.1),xright=quantile(StemOSCPA$bootrep[,1],0.9),ybottom=0,ytop=400,density=50)
abline(v=StemOSCPA$all.out[1],col="red",lwd=2)

#Print quantiles
quantile(StemOSCPA$bootrep[,1],0.1)
quantile(StemOSCPA$bootrep[,1],0.9)
StemOSCPA$all.out[1]

##############################################################################
#Stem density boxplot
##############################################################################
boxplot(rowSums(OSTaxaMatrix1)~RichMspa,notch=T,xlab="MSPA class",main="Overstory stem density by MSPA class",ylab="Stem density")

