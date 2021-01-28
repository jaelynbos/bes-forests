###############################################################################
#RPART (CART) for clusters
#Jaelyn Bos
##############################################################################

##############################################################################
#Prior to running this script, run "Clusters.R"
# Required packages: rpart, rpart.plot
###############################################################################

library(rpart)
library(rpart.plot)

#############################################################################
#Create aspect factor
#############################################################################

AspectBreaks<-c(0,22.5,67.5,112.5,157.5,202.5,247.5,292.5,337.5,380)
AspectBinsGC<-cut(GcClustBGS$aspect,AspectBreaks)
AspectBinsOS<-cut(OsClustBGS$aspect,AspectBreaks)
levels(AspectBinsGC)<-c("N","NE","E","SE","S","SW","W","NW","N")
levels(AspectBinsOS)<-c("N","NE","E","SE","S","SW","W","NW","N")

##############################################################################
#Create blob type factor
##############################################################################

#"S" = single point, "G"= grove, "P" = patch
BlobTypeGC<-rep("S",times=nrow(GcClustBGS))
BlobTypeOS<-rep("S",times=nrow(OsClustBGS))

BlobTypeGC[GcClustBGS$groveID!=-9999]<-"G"
BlobTypeOS[OsClustBGS$groveID!=-9999]<-"G"

BlobTypeGC[GcClustBGS$patchID!=-9999]<-"P"
BlobTypeOS[OsClustBGS$patchID!=-9999]<-"P"

#############################################################################
#Put everything back together, then get relevant columns
##############################################################################
GcClustBGS<-cbind(GcClustBGS,AspectBinsGC,BlobTypeGC)
OsClustBGS<-cbind(OsClustBGS,AspectBinsOS,BlobTypeOS)

BGSvarsGC<-GcClustBGS[,c("Latitude","Longitude","dist2val","dist2edge","elev","slope","mspa","landform_class","AspectBinsGC","BlobTypeGC")]
BGSvarsOS<-OsClustBGS[,c("Latitude","Longitude","dist2val","dist2edge","elev","slope","mspa","landform_class","AspectBinsOS","BlobTypeOS")]

BGSvarsOS<-as.data.frame(BGSvarsOS)
BGSvarsGC<-as.data.frame(BGSvarsGC)

BGSvarsOS$mspa<-as.factor(BGSvarsOS$mspa)
BGSvarsGC$mspa<-as.factor(BGSvarsGC$mspa)

BGSvarsOS$AspectBins<-as.factor(BGSvarsOS$AspectBinsOS)
BGSvarsGC$AspectBins<-as.factor(BGSvarsGC$AspectBinsGC)

BGSvarsOS$BlobType<-as.factor(BGSvarsOS$BlobTypeOS)
BGSvarsGC$BlobType<-as.factor(BGSvarsGC$BlobTypeGC)

BGSvarsOS$landform_class<-as.factor(BGSvarsOS$landform_class)
BGSvarsGC$landform_class<-as.factor(BGSvarsGC$landform_class)

rm(GcClustBGS,OsClustBGS)

#####################################################################################################
#Fit a model
######################################################################################################

fit<-rpart(OSCutree4.5~landform_class+dist2val+dist2edge+slope+mspa+elev+AspectBinsOS+BlobTypeOS+Latitude+Longitude,method="class",data=BGSvarsOS)
rpart.plot(fit,extra=100,cex=0.8)
rsq.rpart(fit)

#Create more models and plots as desired