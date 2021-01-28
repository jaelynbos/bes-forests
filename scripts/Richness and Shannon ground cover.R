################################################################################
#Ground cover species richness and Shannon diversity
#Boxplots and nCPAs
#Jaelyn Bos
################################################################################

################################################################################
#Prior to running this script, run "Create GC community data.R" 
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
#Ground cover species richness nCPA
##############################################################################

GCTaxaOccurance<-apply(GCTaxaMatrix1,c(1,2),occurance)

#Calculate ground cover spp richness
GcRichness<-rowSums(GCTaxaOccurance)

#Calculate minimum distance to edge or valley
GcMins<-pmin(GcAll$dist2edge,GcAll$dist2val)

#Calculate Poisson change point for species richness
RichGcCpa<-cp.pois(GcMins,GcRichness,data=as.data.frame(cbind(GcMins,GcRichness)))

#Plot richness points along min distance to edge gradient
plot(RichGcCpa$xx,RichGcCpa$yy,xlab="Min distance to edge or valley",ylab="Ground cover species richness",main="Poisson change point ground cover species richness")

#Add rectangle with 10th and 90th percentile change points
rect(xleft=quantile(RichGcCpa$bootrep[,1],0.1),xright=quantile(RichGcCpa$bootrep[,1],0.9),ybottom=0,ytop=50,density=50)

#Add line with change point
abline(v=RichGcCpa$all.out[1],col="red",lwd=2)

#Print quantiles
quantile(RichGcCpa$bootrep[,1],0.1)
quantile(RichGcCpa$bootrep[,1],0.9)
RichGcCpa$all.out[1]

##############################################################################
#Ground cover species richness boxplot
##############################################################################
RichMspa<-as.factor(GcAll$mspa)
levels(RichMspa)<-c("other","connected canopy","edge","other","core","connected canopy","edge","connected canopy","edge","other","edge","other")

boxplot(GcRichness~RichMspa,notch=T,xlab="MSPA class",main="Ground cover species richness by MSPA class",ylab="Species richness")

##############################################################################
#Ground cover Shannon diversity nCPA
##############################################################################

#Calculate Shannon diversity index for overstory spp
GcShannon<-diversity(GCTaxaMatrix1,index="shannon")

#Calculate Poisson change point for Shannon diversity index
ShannonGcCpa<-cp.pois(GcMins,GcShannon,data=as.data.frame(cbind(GcMins,GcShannon)))


#Plot Shannon points along min distance to edge gradient
plot(ShannonGcCpa$xx,ShannonGcCpa$yy,xlab="Min distance to edge or valley",ylab="Ground cover Shannon diversity",main="Poisson change point ground cover Shannon diversity")

#Add rectangle with 10th and 90th percentile points
rect(xleft=quantile(ShannonGcCpa$bootrep[,1],0.1),xright=quantile(ShannonGcCpa$bootrep[,1],0.9),ybottom=0,ytop=50,density=50)

#Add line with change point
abline(v=ShannonGcCpa$all.out[1],col="red",lwd=2)

#Print quantiles
quantile(ShannonGcCpa$bootrep[,1],0.1)
quantile(ShannonGcCpa$bootrep[,1],0.9)
ShannonGcCpa$all.out[1]

##############################################################################
#Ground cover Shannon diversity boxplot
###############################################################################

boxplot(GcShannon~RichMspa,notch=T,xlab="MSPA class",main="Ground cover Shannon diversity by MSPA class",ylab="Shannon diversity index")

