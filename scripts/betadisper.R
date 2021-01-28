###############################################################################
#Dispersion metrics
#Jaelyn Bos
###############################################################################

##############################################################################
#Prior to running this script, run "Taxa and environmental data.R"
# Required packages: vegan
###############################################################################

library(vegan)
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

##############################################################################
#Make MSPA factor
##############################################################################

#Overstory
OsClustBGS$mspa<-as.factor(OsClustBGS$mspa)
#Recode into 4 classes
levels(OsClustBGS$mspa)<-c("background","connected canopy","edge","background","core","connected canopy","edge","connected canopy","edge","background","edge","background")

#Ground cover
GcClustBGS$mspa<-as.factor(GcClustBGS$mspa)
levels(GcClustBGS$mspa)<-c("background","connected canopy","edge","background","core","connected canopy","edge","connected canopy","edge","background","edge")

##############################################################################
#Run betadisper for MSPA class
##############################################################################

#overstory
OsDisp<-betadisper(BrayCurtisOS,OsClustBGS$mspa)

#Ground cover
GcDisp<-betadisper(BrayCurtisGC,GcClustBGS$mspa)

#And make boxplot
boxplot(OsDisp,notch=T,main="Overstory dispersion by MSPA class",xlab="MSPA class")

boxplot(GcDisp,notch=T,main="Ground cover dispersion by MSPA class",xlab="MSPA class",with=table(GcClustBGS$mspa))

#And ANOVA
anova(OsDisp)

############################################################################
#Run betadisper for cluster
#############################################################################

#overstory
OsDispC<-betadisper(BrayCurtisOS,OSCutree4.5)

#And make boxplot
boxplot(OsDispC,notch=T,main="Overstory dispersion by cluster",xlab="Cluster #")

