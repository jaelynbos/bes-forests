###############################################################################
# Non metric multi dimensional scaling for overstory sites and species
#Jaelyn Bos
###############################################################################

###############################################################################
#Prior to running this script, run "Create OS community data.R" and "Clusters.R"
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
#Overstory NMDS
###############################################################################

#Remove two mis-coded taxa
OSTaxaMatrix.1<-OSTaxaMatrix[,-c(29,58,94)]

#Remove empty rows and columns
OSTaxaMatrix.1<-OSTaxaMatrix.1[!(rowSums(OSTaxaMatrix.1)==0),!(colSums(OSTaxaMatrix.1)==0)]

#Run NMDS
OsNmds<-metaMDS(OSTaxaMatrix.1,try=20,trymax=100)

#Plot results
plot(OsNmds)
text(OsNmds)

#Remove most distinct site (CLIFTON14, has only Tsuga canadensis and Magnolia)
OSTaxaMatrix.1<-OSTaxaMatrix.1[-440,]

OsNmds1<-metaMDS(OSTaxaMatrix1,try=20,trymax=100)
plot(OsNmds1)
text(OsNmds1)

#Remove several of the most distinct sites
#Sites to eliminate: SWAPRK 10, FRIENDS 6,MORPAR 38, HOLBRD2 13,HERUN3 55, Hockley Rd. Pt. 11
#583,207,566,551,536,693
OSTaxaMatrix.3<-OSTaxaMatrix.1[-c(583,207,566,551,536,693),]

#Run the NMDS
OsNmds2<-metaMDS(OSTaxaMatrix.3,try=20,trymax=100)

#And run with k=3
OsNmds2.3D<-metaMDS(OSTaxaMatrix.3,try=20,trymax=100,k=3)

#Match sites with environmental data
OSTaxaMatrix.4<-cbind(rownames(OSTaxaMatrix.3),OSTaxaMatrix.3)
colnames(OSTaxaMatrix.4)<-c("Plot_Name",colnames(OSTaxaMatrix.3))
OSTaxaBGS<-left_join(as.data.frame(OSTaxaMatrix.4),as.data.frame(BGS2020))

##############################################################################
#Make plot colored by mindist
#############################################################################

#Calculate minimum distance to edge or valley
mindist<-pmin(OSTaxaBGS$dist2edge,OSTaxaBGS$dist2val)

qplot(OsNmds2$points[,1],OsNmds2$points[,2],colour=mindist,xlab="x",ylab="y",label=rownames(OsNmds2$points),geom=c("text"),main="Overstory species subset")+scale_color_viridis(option = "D")

##############################################################################
#Make plot colored by MSPA class
##############################################################################
OSTaxaBGS$mspa<-as.factor(OSTaxaBGS$mspa)
levels(OSTaxaBGS$mspa)<-c("background","connected canopy","edge","background","core","connected canopy","edge","connected canopy","edge","background","edge")

qplot(OsNmds2$points[,1],OsNmds2$points[,2],colour=OSTaxaBGS$mspa,xlab="x",ylab="y",label=rownames(OsNmds4$points),geom=c("point"),main="Overstory sites subset",xlim=c(-0.012,0.01),ylim=c(-0.007,0.007))+scale_color_manual(values=c("grey40","red","black","green"))

##############################################################################
#Make plot colored by cluster affiliation
#############################################################################

OSClusters<-left_join(as.data.frame(OSTaxaMatrix4),as.data.frame(OSCutree5.5))
OSClusters<-OSClusters$clust

qplot(OsNmds4$points[,1],OsNmds4$points[,2],colour=OSClusters,xlab="x",ylab="y",label=rownames(OsNmds4$points),geom=c("text"),main="Overstory sites subset")+scale_color_viridis(option = "D",discrete=T)


#############################################################################
#Make plot colored by landform
#############################################################################

OSTaxaBGS$landform<-as.factor(OSTaxaBGS$landform)

qplot(OsNmds2$points[,1],OsNmds2$points[,2],colour=OSTaxaBGS$landform,xlab="x",ylab="y",label=rownames(OsNmds4$points),geom=c("point"),main="Overstory sites subset",xlim=c(-0.012,0.01),ylim=c(-0.007,0.007))+v
#############################################################################
#Make species plot colored by is.quercus
#############################################################################

IsQuercus<-vector(length=nrow(OsNmds2$species))
IsQuercus[grep("QU",rownames(OsNmds2$species))]<-TRUE
IsQuercus[grep("FAGR",rownames(OsNmds2$species))]<-TRUE

qplot(OsNmds4$species[,1],OsNmds4$species[,2],colour=IsQuercus,xlab="x",ylab="y",label=rownames(OsNmds4$species),geom=c("text"),main="Overstory species subset",xlim=c(-0.012,0.01),ylim=c(-0.007,0.007))

##############################################################################
#Scree plot
##############################################################################


OSTaxaMatrix3<-OSTaxaMatrix1[-c(583,207,566,551,536,693),-c(91,29)]
OSTaxaMatrix4<-cbind(rownames(OSTaxaMatrix3),OSTaxaMatrix3)

OsNmdsk1<-metaMDS(OSTaxaMatrix.3,try=20,trymax=30,k=1)
OsNmdsk2<-metaMDS(OSTaxaMatrix.3,try=20,trymax=20,k=2)
OsNmdsk3<-metaMDS(OSTaxaMatrix.3,try=20,trymax=20,k=3)
OsNmdsk4<-metaMDS(OSTaxaMatrix.3,try=20,trymax=20,k=4)
OsNmdsk5<-metaMDS(OSTaxaMatrix.3,try=20,trymax=20,k=5)
OsNmdsk6<-metaMDS(OSTaxaMatrix.3,try=20,trymax=20,k=6)
OsNmdsk7<-metaMDS(OSTaxaMatrix.3,try=20,trymax=20,k=7)
OsNmdsk8<-metaMDS(OSTaxaMatrix.3,try=20,trymax=20,k=8)
OsNmdsk9<-metaMDS(OSTaxaMatrix.3,try=20,trymax=20,k=9)
OsNmdsk10<-metaMDS(OSTaxaMatrix.3,try=20,trymax=20,k=10)

OsNmdsStress<-cbind(OsNmdsk1$stress,OsNmdsk2$stress,OsNmdsk3$stress,OsNmdsk4$stress,OsNmdsk5$stress,OsNmdsk6$stress,OsNmdsk7$stress,OsNmdsk8$stress,OsNmdsk9$stress,OsNmdsk10$stress)

plot(rep(1:10),OsNmdsStress,xlab="k=",ylab="stress",main="Stress by NMDS dimensions overstory")