###############################################################################
#Ground cover TITAN
#Jaelyn Bos
################################################################################

###############################################################################
#Prior to running this script, run "Create GC community data.R" and
#"Taxa and environmental data.R"
#Required packages: TITAN2
###############################################################################

library(TITAN2)

#Check species names
colnames(GCTaxaMatrix)

#Remove taxa occurring in fewer than 3 sites
occurance<-function(x){
	if (x>0){
	return(1)
	}
	else{
	return(0)
	}
}

TaxaOccurance<-apply(GCTaxaMatrix1,c(1,2),occurance)
GCTaxaForTitan<-GCTaxaMatrix1[,colSums(TaxaOccurance)>2]

#Calculate minimum distance to natural or manmade edge
GcMins<-pmin(GcAll$dist2edge,GcAll$dist2val)

GcTitanMins<-titan(env=GcMins,txa=GCTaxaForTitan,ncpus=4)
GcTitanEdge<-titan(env=GcAll$dist2edge,txa=GCTaxaForTitan,ncpus=4)



