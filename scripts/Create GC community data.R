#############################################################################
#Create a matrix of groundcover taxa
#Jaelyn Bos
#27 February 2020
#############################################################################

#############################################################################
#Before running this script, run "USFS codes.R"
#Required packages: stringr
#############################################################################

library(stringr) 

#Load data

#This file includes all the data prior to 2018
setwd("C:/Users/Jaelyn Bos/Documents/Baltimore Forestry data/2018")
GroundCover2018<-read.csv("Ground CoverBGS_CCCARLY.csv")
#Makes percent cover column numeric
GroundCover2018[,4]<-as.numeric(GroundCover2018[,4])
#Trim white space from site name column
GroundCover2018[,2]<-trimws(GroundCover2018[,2])

#Remove non-sampled sites
GroundCover2018<-GroundCover2018[!is.na(GroundCover2018[,4]),]

#This file includes the (presumed) 2018 data, but formatted differently
GroundCover2018B<-read.csv("Ground cover 2018B.csv")

#Get rid of unnecessary columns
GroundCover2018B<-GroundCover2018B[,1:18]

#Trim white space from plot names
GroundCover2018B[,1]<-trimws(GroundCover2018B[,1])

#Include only sampled sites
GroundCover2018B<-GroundCover2018B[!(GroundCover2018B[,2]==""),]

#This file includes the 2019 data
setwd("C:/Users/Jaelyn Bos/Documents/Baltimore Forestry data/2019")
GroundCover2019<-read.csv("Ground Cover 2019.csv")
#Makes percent cover column numeric
GroundCover2019[,4]<-as.numeric(GroundCover2019[,4])

#Trim white space from site name column
GroundCover2019[,2]<-trimws(GroundCover2019[,2])

#############################################################################
#This section is all data prep for the data sets other than 2018B. 
#2018B is formatted differently and treated in the next section
##########################################################################

GCMatrixA<-rbind(GroundCover2018,GroundCover2019)

#Standardize the "less than 1%" column from different data sources
GCMatrixA[,5]<-toupper(as.character(GCMatrixA[,5]))

#Function to change the <1% cover taxa to 0.5%

percent.corrected<-function(Percents){
	correct<-as.numeric(Percents[1])
	if(as.character(Percents[2])=="YES"){
		correct<-0.5
	}
	return(correct)
}

#Apply the function to create a vector
Percent.Cover.Corrected<-GCMatrixA[,4:5]

#Create a vector of the corrected values
Percent.Cover.Corrected<-apply(Percent.Cover.Corrected,1,percent.corrected)

#Add the corrected percent back to the matrix
GCMatrixA<-cbind(GCMatrixA,Percent.Cover.Corrected)

#Clean up workspace
rm(Percent.Cover.Corrected)

#Name sites
GroundCoverSitesA<-unique(trimws(as.character(GCMatrixA$Point.Code)))

GCTaxa<-c(as.character(unlist(GroundCover2018B[,3:10])),as.character(GCMatrixA$Species.Code))
GCTaxa<-sort(unique(trimws(GCTaxa)))
GCTaxa<-unique(usfs.codes(GCTaxa))

UnIDGrepList<-c("UNI","UNK","Uni","Unk","N/A","DEAD","Dead","DEBRIS")
UnknownSpp<-as.numeric(unlist(sapply(UnIDGrepList,grep,x=GCTaxa)))

#Delete unknown specices from species list
GCTaxa<-GCTaxa[-UnknownSpp]

#Delete unknowns "" and "??"
GCTaxa<-GCTaxa[-(c(1,2))]


############################################################################
#Format the 2018B data
############################################################################

#All numeric columns are factors, have to be re-coded as as.numeric(as.character())

GCSites2018B<-trimws(as.character(unique(GroundCover2018B[,1])))

GCSpp2018B<-GroundCover2018B[,3:10]
GCSpp2018B<-apply(GCSpp2018B,c(1,2),usfs.codes)

GCCounts2018B<-GroundCover2018B[,11:18]

GCArray2018B<-abind(GCSpp2018B,GCCounts2018B,along=3)

GCTaxaMatrixB<-matrix(nrow=length(GCSites2018B),ncol=length(GCTaxa))

for (i in 1:length(GCSites2018B)){
	a<-(as.character(GroundCover2018B[,1])==GCSites2018B[i])
	TaxaBySite<-GCArray2018B[a,,]
	for (j in 1:length(GCTaxa)){
		y<-as.numeric(as.character(TaxaBySite[TaxaBySite[,1]==GCTaxa[j],2]))
			if(length(y)==0){
				GCTaxaMatrixB[i,j]<-0
			}
			else {
		GCTaxaMatrixB[i,j]<-y
			}
	}
rm(y,TaxaBySite)
}

rownames(GCTaxaMatrixB)<-GCSites2018B

GCTaxaMatrixB<-GCTaxaMatrixB[!is.na(rowSums(GCTaxaMatrixB)),]

#########################################################################
#Create empty matrix to hold reformatted data

GCTaxaMatrixA<-matrix(nrow=length(GroundCoverSitesA),ncol=length(GCTaxa),data=0)

for (i in 1:length(GroundCoverSitesA)){
	for (j in 1:length(GCTaxa)){
		SiteA<-GCMatrixA[GCMatrixA[,2]==GroundCoverSitesA[i],]
		cellvalue<-SiteA[SiteA[,3]==GCTaxa[j],6]
		if (length(cellvalue)==0){
			cellvalue<-0
			}
		if(length(cellvalue)>0){
			cellvalue<-max(cellvalue)
					}
		GCTaxaMatrixA[i,j]<-cellvalue
		rm(SiteA,cellvalue)
	}
}
rownames(GCTaxaMatrixA)<-GroundCoverSitesA
rownames(GCTaxaMatrixA)<-str_replace(rownames(GCTaxaMatrixA),"Knoll ","Knoll Pt. ")


GCTaxaMatrix<-rbind(GCTaxaMatrixA,GCTaxaMatrixB)
colnames(GCTaxaMatrix)<-GCTaxa

#Remove duplicate rows
GCTaxaMatrix<-GCTaxaMatrix[!duplicated(rownames(GCTaxaMatrix)),]

#Rename two miss-named sites
rownames(GCTaxaMatrix)<-str_replace(rownames(GCTaxaMatrix),"Glen Plot 2/ Pt.","Glen Plot 2 Pt.")
rownames(GCTaxaMatrix)<-str_replace(rownames(GCTaxaMatrix),"Glen Plot 3/ Pt.","Glen Plot 3 Pt.")
