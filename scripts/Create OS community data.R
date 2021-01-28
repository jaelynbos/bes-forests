#############################################################################
#Create a matrix of overstory taxa
#Jaelyn Bos
#27 February 2020
#############################################################################

#############################################################################
#Before running this script, run "USFS codes.R"
#Required packages: stringr
##############################################################################

library(stringr)

#Set working directory for 2018 data
setwd("C:/Users/Jaelyn Bos/Documents/Baltimore Forestry data/2018")

#Load 2018 data
Prism2018B<-read.csv("PrismData_2018B.csv")

#Remove schumaker/other irrelevant columns
Prism2018B<-Prism2018B[,1:21]

#Get rid of rows with missing dates
Prism2018B<-Prism2018B[!(Prism2018B$Date==""),]

#Load pre-2018 data
setwd("C:/Users/Jaelyn Bos/Documents/Baltimore Forestry data/Pre2018")
Prism2017<-read.csv("Prism Data.csv")

#Rename columns
colnames(Prism2017)<-c("FID","Stems","Species.Code","Schumaker","Point.Code","SchumakerA")

#Reorder columns
Prism2017<-Prism2017[,c(1,5,2,3,4)]

#Load 2019 data
setwd("C:/Users/Jaelyn Bos/Documents/Baltimore Forestry data/2019")
Prism2019<-read.csv("PrismData2019.csv")

#Rename columns
colnames(Prism2019)<-c("FID","Point.Code","Stems","Species.Code","Schumaker")

#Rbind pre-2018 and 2019 data
PrismMatrix<-rbind(Prism2017,Prism2019)

#Make stem counts numeric
PrismMatrix[,3]<-as.numeric(PrismMatrix[,3])

#Remove NA sites
PrismMatrix<-PrismMatrix[!is.na(PrismMatrix[,3]),]

#Make vector of all species names
PrismTaxa<-c(as.character(unlist(Prism2018B[,3:21])),as.character(PrismMatrix$Species.Code))

#Order species names, trim white space, and delete duplicates
PrismTaxa<-sort(unique(trimws(PrismTaxa)))

#Rename the miss-coded species
PrismTaxa<-unique(usfs.codes(PrismTaxa))

#Delete unknown specices from species list
UnIDGrepList<-c("UNI","UNK","Uni","Unk","N/A","DEAD","Dead")
UnknownSpp<-as.numeric(unlist(sapply(UnIDGrepList,grep,x=PrismTaxa)))
PrismTaxa<-PrismTaxa[-UnknownSpp]
rm(UnknownSpp)
length(PrismTaxa)

#Make vector of point codes for 2017/2019 data
PrismSites<-unique(trimws(as.character(PrismMatrix$Point.Code)))

#Make stems column numeric
PrismMatrix[,3]<-as.numeric(PrismMatrix[,3])
PrismMatrix<-PrismMatrix[!is.na(PrismMatrix[,3]),]

#Make blank matrix to hold results
PrismTaxaMatrix<-matrix(nrow=length(PrismSites),ncol=length(PrismTaxa),data=0)

for (i in 1:length(PrismSites)){
	for (j in 1:length(PrismTaxa)){
		SiteA<-PrismMatrix[PrismMatrix[,2]==PrismSites[i],]
		cellvalue<-SiteA[SiteA[,4]==PrismTaxa[j],3]
		if (length(cellvalue)==0){
			cellvalue<-0
			}
		else	{
			cellvalue<-sum(cellvalue)
					}
		PrismTaxaMatrix[i,j]<-cellvalue
		rm(SiteA,cellvalue)
	}
}
rownames(PrismTaxaMatrix)<-PrismSites

#Prism count function
prism.count<- function(Species,Site){
	a<-as.vector(table(Site==Species))[2]
	if (is.na(a)){
		return(0)
	}
	else{
		return(a)
	}
}

#Make empty matrix to hold 2018 results
PrismTaxaMatrix2018<-matrix(nrow=nrow(Prism2018B),ncol=length(PrismTaxa),data=0)

#Fill matrix
for(i in 1:nrow(Prism2018B)){
	for (j in 1:length(PrismTaxa)){
		PrismTaxaMatrix2018[i,j]<-prism.count(PrismTaxa[j],Prism2018B[i,])
	}
}

#Name columns
colnames(PrismTaxaMatrix2018)<-PrismTaxa
rownames(PrismTaxaMatrix2018)<-Prism2018B[,1]
#Combine all the taxa datasets
OSTaxaMatrix<-rbind(PrismTaxaMatrix,PrismTaxaMatrix2018)

rownames(OSTaxaMatrix)<-str_replace(rownames(OSTaxaMatrix),"Glen Plot 2/ Pt.","Glen Plot 2 Pt.")
rownames(OSTaxaMatrix)<-str_replace(rownames(OSTaxaMatrix),"Glen Plot 3/ Pt.","Glen Plot 3 Pt.")

#Remove empty column
OSTaxaMatrix<-OSTaxaMatrix[,-1]
