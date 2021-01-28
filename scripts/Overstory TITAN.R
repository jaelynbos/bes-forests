###############################################################################
#Overstory TITAN
#Jaelyn Bos
################################################################################

###############################################################################
#Prior to running this script, run "Create OS community data.R"
#Required packages: dplyr, TITAN2
###############################################################################

library(dplyr)
library(TITAN2)

#Check species names
colnames(OSTaxaMatrix)

#Remove taxa occurring in fewer than 3 sites
occurance<-function(x){
	if (x>0){
	return(1)
	}
	else{
	return(0)
	}
}

TaxaOccurance<-apply(OSTaxaMatrix1,c(1,2),occurance)
OSTaxaForTitan<-OSTaxaMatrix1[,colSums(TaxaOccurance)>2]

OsMins<-pmin(OsAll$dist2edge,OsAll$dist2val)

OsTitanMins<-titan(env=OsMins,txa=OSTaxaForTitan,ncpus=4)
OsTitanEdge<-titan(env=OsAll$dist2edge,txa=OSTaxaForTitan,ncpus=4)

rm(TaxaOccurance,x)
