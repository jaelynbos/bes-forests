###############################################################################
# Non metric multi dimensional scaling for ground cover sites and species
#Jaelyn Bos
###############################################################################

###############################################################################
#Prior to running this script, run "Create GC community data.R" and "Clusters.R"
#Required packages: vegan, dyplyr, ggplot2, viridis
################################################################################

library(vegan)
library(dplyr)
library(ggplot2)
library(viridis)

setwd("C:/Users/Jaelyn Bos/Documents/Baltimore Forestry data")
BGS2020<-read.csv("BGS_coords2020_attributes.csv")

#Remove duplicate rows
BGS2020<-BGS2020[!duplicated(BGS2020[,2]),]

###############################################################################
#Ground cover NMDS
###############################################################################

#Remove empty rows and columns
GCTaxaMatrix.1<-GCTaxaMatrix[!(rowSums(GCTaxaMatrix)==0),!(colSums(OSTaxaMatrix)==0)]

#Run NMDS
GcNmds<-metaMDS(GCTaxaMatrix.1,try=20,trymax=100)

#Plot results
plot(GcNmds)
text(GcNmds)

#Remove most distinct sites
GCTaxaMatrix.2<-GCTaxaMatrix.1[-c(679,185,432,320,852,618),]

#Run NMDS again
GcNmds2<-metaMDS(GCTaxaMatrix.2,try=20,trymax=100)

#Attach environmental variables
GCTaxaMatrix.3<-cbind(rownames(GCTaxaMatrix.2),GCTaxaMatrix.2)
colnames(GCTaxaMatrix.3)<-c("Plot_Name",colnames(GCTaxaMatrix.2))
GCTaxaBGS<-left_join(as.data.frame(GCTaxaMatrix.3),as.data.frame(BGS2020))

##############################################################################
#Make plot colored by mindist
#############################################################################

#Calculate minimum distance to edge or valley
mindist<-pmin(GCTaxaBGS$dist2edge,GCTaxaBGS$dist2val)

#Make scatter plot colored by mindist
qplot(GcNmds2$points[,1],GcNmds2$points[,2],colour=mindist,xlab="x",ylab="y",label=rownames(GcNmds2$points),geom=c("text"),main="Ground cover sites subset")+scale_color_viridis(option = "D")

##############################################################################
#Make plot colored by MSPA class
##############################################################################
OSTaxaBGS$mspa<-as.factor(OSTaxaBGS$mspa)
levels(OSTaxaBGS$mspa)<-c("background","connected canopy","edge","background","core","connected canopy","edge","connected canopy","edge","background","edge")

qplot(GcNmds2$points[,1],GcNmds2$points[,2],colour=GCTaxaBGS$mspa,xlab="x",ylab="y",label=rownames(GcNmds2$points),geom=c("text"),main="Ground cover sites subset")+scale_color_manual(values=c("grey40","red","black","green"))

#############################################################################
#Make plot colored by landform
#############################################################################

GCTaxaBGS$landform<-as.factor(GCTaxaBGS$landform)

qplot(GcNmds2$points[,1],GcNmds2$points[,2],colour=GCTaxaBGS$landform,xlab="x",ylab="y",label=rownames(GcNmds2$points),geom=c("text"),main="Ground cover sites subset")+scale_color_manual(values=c("navy","grey","yellow","green","red","orange","yellow","beige","maroon","blue","grey"))

##############################################################################
#Scree plot
#############################################################################

GcNmdsk1<-metaMDS(GCTaxaMatrix.3,try=20,trymax=30,k=1)
GcNmdsk2<-metaMDS(GCTaxaMatrix.3,try=20,trymax=20,k=2)
GcNmdsk3<-metaMDS(GCTaxaMatrix.3,try=20,trymax=20,k=3)
GcNmdsk4<-metaMDS(GCTaxaMatrix.3,try=20,trymax=20,k=4)
GcNmdsk5<-metaMDS(GCTaxaMatrix.3,try=20,trymax=20,k=5)
GcNmdsk6<-metaMDS(GCTaxaMatrix.3,try=20,trymax=20,k=6)
GcNmdsk7<-metaMDS(GCTaxaMatrix.3,try=20,trymax=20,k=7)
GcNmdsk8<-metaMDS(GCTaxaMatrix.3,try=20,trymax=20,k=8)
GcNmdsk9<-metaMDS(GCTaxaMatrix.3,try=20,trymax=20,k=9)
GcNmdsk10<-metaMDS(GCTaxaMatrix.3,try=20,trymax=20,k=10)


GcNmdsStress<-cbind(GcNmdsk1$stress,GcNmdsk2$stress,GcNmdsk3$stress,GcNmdsk4$stress,GcNmdsk5$stress,GcNmdsk6$stress,GcNmdsk7$stress,GcNmdsk8$stress,GcNmdsk9$stress,GcNmdsk10$stress)
plot(rep(1:10),GcNmdsStress,xlab="k=",ylab="stress",main="Stress by NMDS dimensions ground cover")