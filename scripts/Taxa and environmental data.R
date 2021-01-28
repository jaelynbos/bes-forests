##############################################################################
#Taxa and environmental data
#Jaelyn Bos
###############################################################################

###############################################################################
#Prior to running this script, run "Create GC community data.R" and
#"Create OS community data.R"
#Required packages: dplyr
###############################################################################

library(dplyr)

#Load environment data
setwd("C:/Users/Jaelyn Bos/Documents/Baltimore Forestry data")
BGS2020<-read.csv("BGS_coords2020_attributes.csv")

#Remove duplicate rows
BGS2020<-BGS2020[!duplicated(BGS2020[,2]),]

#Make site names into column for ground cover
GCTaxaMatrix1<-cbind(rownames(GCTaxaMatrix),GCTaxaMatrix)
colnames(GCTaxaMatrix1)<-c("Plot_Name",colnames(GCTaxaMatrix))

#Make site names into column for overstory
OSTaxaMatrix1<-cbind(rownames(OSTaxaMatrix),OSTaxaMatrix)
colnames(OSTaxaMatrix1)<-c("Plot_Name",colnames(OSTaxaMatrix))

#Left join the taxa and environmental data
GcAll<-left_join(as.data.frame(GCTaxaMatrix1),as.data.frame(BGS2020))
OsAll<-left_join(as.data.frame(OSTaxaMatrix1),as.data.frame(BGS2020))

#Remove funky sites from overstory
OsAll<-OsAll[!(OsAll$dist2edge==-9999),]
OsAll<-OsAll[!is.na(OsAll$dist2edge),]

#Taxa only 
GCTaxaMatrix1<-GcAll[,2:(ncol(GCTaxaMatrix)+1)]
OSTaxaMatrix1<-OsAll[,2:(ncol(OSTaxaMatrix)+1)]

#Make everything numeric
GCTaxaMatrix1<-apply(GCTaxaMatrix1,c(1,2),function(x) as.numeric(as.character(x)))
OSTaxaMatrix1<-apply(OSTaxaMatrix1,c(1,2),function(x) as.numeric(as.character(x)))

#Outputs:

#OsAll includes overstory taxa + point attributes
#GcAll includes groundcover taxa + point attributes

#OSTaxaMatrix1 is a matrix of overstory spp counts, including only points
#with associated attributes

#GCTaxaMatrix1 is the same for ground cover
