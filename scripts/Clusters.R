###############################################################################
#Cluster analysis
#Jaelyn Bos
##############################################################################

##############################################################################
#Prior to running this script, run "Taxa and environmental data.R"
# Required packages: vegan,labdsv (for indvals)
###############################################################################

library(vegan)
library(labdsv)

GCForClust1<-GCTaxaMatrix1
OSForClust1<-OSTaxaMatrix1

#Remove empty rows and columns
GcClustBGS<-GcAll[!(rowSums(GCForClust1)==0),]
GCForClust1<-GCForClust1[!(rowSums(GCForClust1)==0),]
GCForClust1<-GCForClust1[,!(colSums(GCForClust1)==0)]

OsClustBGS<-OsAll[!(rowSums(OSForClust1)==0),]
OSForClust1<-OSForClust1[!(rowSums(OSForClust1)==0),]
OSForClust1<-OSForClust1[,!(colSums(OSForClust1)==0)]

#Calculate Bray-Curtis distances for all sites
BrayCurtisGC<-vegdist(GCForClust1,method="bray")
BrayCurtisOS<-vegdist(OSForClust1,method="bray")

#And cluster
ClusteredGC<-hclust(BrayCurtisGC,method="ward.D2")
ClusteredOS<-hclust(BrayCurtisOS,method="ward.D2")

#Plot results
plot(ClusteredGC,labels=F,main="Ground cover clusters")
plot(ClusteredOS,labels=F,main="Overstory clusters")

###########################################################################
#Cut trees to create clusters
##########################################################################

#4 cluster overstory solution
OSCutree4.5<-cutree(ClusteredOS,h=4.5)

#5 cluster ground cover solution
GCCutree3.7<-cutree(ClusteredGC,h=3.7)

############################################################################
#Comparing indvals across clusters
############################################################################

#Calculate indvals for clusters
OSindval4.5<-indval(OSForClust1,OSCutree4.5)
GCindval3.7<-indval(GCForClust1,GCCutree3.7)

#Pull out indvals for species in each cluster
indval.report<-function(IndvalObject,ClusterNumber,rank)
{
a<-ClusterNumber
io<-IndvalObject
b<-which(io$indval[,a]==sort(io$indval[,a],decreasing=T)[rank],arr.ind=T)
return(c(colnames(GcHerbs)[b],io$indval[,a][b]))
}

