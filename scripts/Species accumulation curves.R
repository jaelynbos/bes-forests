#############################################################################
#Species accumulation curves by growth form for both overstory and ground cover
#Divided by MSPA class
#Jaelyn Bos
#############################################################################

##############################################################################
#Prior to running this script, run "Taxa and environmental data.R" 
#Required packages: vegan, dplyr,tidyr
##############################################################################
library(dplyr)
library(vegan)
library(tidyr)

#Objects from other scripts: GCTaxaMatrix,GCTaxaMatrix1,GcAll,
#OSTaxaMatrix,OSTaxaMatrix1,OsAll

#Get spp info
setwd("C:/Users/Jaelyn Bos/Documents/Baltimore Forestry data/2019")
SppInfo<-read.csv("SppID2019.csv")

#Left join spp names to characteristics
GcTaxaNames<-colnames(GCTaxaMatrix)
OsTaxaNames<-colnames(OSTaxaMatrix)
GcTaxa<-left_join(as.data.frame(GcTaxaNames),as.data.frame(SppInfo),by=c("GcTaxaNames"="Species.Code"))
OsTaxa<-left_join(as.data.frame(OsTaxaNames),as.data.frame(SppInfo),by=c("OsTaxaNames"="Species.Code"))

#Filter out individual growth forms. NAs code as false
vines<-GcTaxa$Vegetation.Type=="Vine"
vines<-replace_na(vines,FALSE)
trees<-OsTaxa$Vegetation.Type=="Tree"
trees<-replace_na(trees,FALSE)
herbs<-GcTaxa$Vegetation.Type=="Herb"
herbs<-replace_na(herbs,FALSE)
shrubs<-GcTaxa$Vegetation.Type=="Shrub"
shrubs<-replace_na(shrubs,FALSE)

#Sort points by MSPA class
GcCore<-(GcAll$mspa==17)
GcEdge<-(GcAll$mspa %in% c(3,35,67,105))
OsCore<-(OsAll$mspa==17)
OsEdge<-(OsAll$mspa %in% c(3,35,67,105))

EdgeVines<-apply(GCTaxaMatrix1[GcEdge,vines],c(1,2),round)
CoreVines<-apply(GCTaxaMatrix1[GcCore,vines],c(1,2),round)

EdgeShrubs<-apply(GCTaxaMatrix1[GcEdge,shrubs],c(1,2),round)
CoreShrubs<-apply(GCTaxaMatrix1[GcCore,shrubs],c(1,2),round)

EdgeHerbs<-apply(GCTaxaMatrix1[GcEdge,herbs],c(1,2),round)
CoreHerbs<-apply(GCTaxaMatrix1[GcCore,herbs],c(1,2),round)

EdgeTrees<-apply(GCTaxaMatrix1[GcEdge,trees],c(1,2),round)
CoreTrees<-apply(GCTaxaMatrix1[GcCore,trees],c(1,2),round)

#Plot species accumulation curves for herbs and vines
plot(specaccum(EdgeHerbs,method="rarefaction"),main="Rarefaction divided by MSPA class")
lines(specaccum(CoreHerbs,method="rarefaction"),col="blue")
lines(specaccum(EdgeVines,method="rarefaction"),col="green")
lines(specaccum(CoreVines,method="rarefaction"),col="yellow")
legend("bottomright",legend=c("Edge herbs","Core herbs","Edge vines","Core vines"),fill=c("black","blue","green","yellow"))

#Plot species accumulation curves for trees and shrubs
plot(specaccum(EdgeTrees,method="rarefaction"),main="Rarefaction divided by MSPA class")
lines(specaccum(CoreTrees,method="rarefaction"),col="blue")
lines(specaccum(CoreShrubs,method="rarefaction"),col="green")
lines(specaccum(EdgeShrubs,method="rarefaction"),col="yellow")
legend("bottomright",legend=c("Edge trees","Core trees","Edge shrubs","Core shrubs"),fill=c("black","blue","green","yellow"))








