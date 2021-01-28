##############################################################################
#Various pie charts
#Jaelyn Bos
###############################################################################

##############################################################################
# Prior to running this script, run "Clusters.R"
##############################################################################

#Create pie charts of species by cluster
cluster.pie<-function(ClusterData,ClusterObject,ClusterNumber,NumberOfTaxa){
	cd<-ClusterData
	co<-ClusterObject
	cn<-ClusterNumber
	nt<-NumberOfTaxa
	p<-sort(colSums(cd[co==cn,]),decreasing=T)
	a<-sum(p[(nt+1):length(p)])
	p<-c(p[1:nt],a)
	print(pie(p,main=c("Overstory species composition cluster #",cn)))
}

#Example: create pie chart of top 14 species for overstory cluster #1
cluster.pie(OSForClust1,OSCutree4.5,1,14)


#############################################################################
# Pie charts showing MSPA class by cluster
#############################################################################

table(OSCutree4.5)

#Overstory
OsClustBGS$mspa<-as.factor(OsClustBGS$mspa)
#Recode into 4 classes
levels(OsClustBGS$mspa)<-c("background","connected canopy","edge","background","core","connected canopy","edge","connected canopy","edge","background","edge","background")

pie(table(OsClustBGS$mspa[OSCutree4.5==1]),main="MSPA classes cluster #1")

##############################################################################
#Pie charts showing MSPA class by site
#############################################################################

site<-str_split(OsClustBGS$Plot_Name," ")

site<-unlist(lapply(site,function(x) x[1]))

pie(table(OSCutree4.5[site=="AH"]))



