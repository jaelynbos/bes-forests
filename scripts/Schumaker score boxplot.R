##############################################################################
#Schumaker score boxplot and nCPA
#Jaelyn Bos
##############################################################################

##############################################################################
#Required packages: dplyr, stringr
#############################################################################


#############################################################################
#Make boxplot by MSPA classes
##############################################################################

library(dplyr)
library(stringr)

#Pre-2018 schumaker score
setwd("C:/Users/Jaelyn Bos/Documents/Baltimore Forestry data/Pre2018")
Sch2017<-read.csv("Prism Data.csv")
Sch2017<-Sch2017[,c(5,4)]
Sch2017<-Sch2017[!is.na(Sch2017[,2]),]
Sch2017<-aggregate(Sch2017[,2],by=list(Sch2017[,1]),FUN=mean)

#2018 schumaker scores
setwd("C:/Users/Jaelyn Bos/Documents/Baltimore Forestry data/2018")
Sch2018<-read.csv("PrismData_2018B.csv")
Sch2018<-Sch2018[,c(1,22:64)]
s<-apply(Sch2018[,2:44],c(1,2),as.character)
s<-apply(s,c(1,2),as.numeric)
s<-apply(s,1,mean,na.rm=T)
Sch2018<-cbind(Sch2018[,1],s)

#2019 schumaker scores
setwd("C:/Users/Jaelyn Bos/Documents/Baltimore Forestry data/2019")
Sch2019<-read.csv("PrismData2019.csv")
s<-as.character(Sch2019[,5])

s<-str_replace(s,"1A","1")
s<-str_replace(s,"2A","2")
s<-str_replace(s,"3A","3")
s<-str_replace(s,"4A","4")
s<-str_replace(s,"5A","5")

Sch2019[,5]<-as.numeric(s)
rm(s)

Sch2019<-Sch2019[,c(2,5)]
Sch2019<-Sch2019[!is.na(Sch2019[,2]),]
Sch2019<-aggregate(Sch2019[,2],by=list(Sch2019[,1]),FUN=mean)

colnames(Sch2017)<-c("Plot_Name","SchMean")
colnames(Sch2018)<-c("Plot_Name","SchMean")
colnames(Sch2019)<-c("Plot_Name","SchMean")

Schumaker<-rbind(Sch2017,Sch2018,Sch2019)
Schumaker<-Schumaker[!is.na(Schumaker[,1]),]

#Join with BGS data
SchBGS<-left_join(Schumaker,BGS2020)

#Remove NAs
SchBGS<-SchBGS[!(SchBGS$mspa==-9999),]

#Make MSPA factor
SchBGS$mspa<-as.factor(SchBGS$mspa)

#Recode MSPA factor
levels(SchBGS$mspa)<-c("other","connected canopy","edge","other","core","connected canopy","edge","connected canopy","edge","other","edge")

boxplot(as.numeric(as.character(SchBGS[,2]))~SchBGS[,14],width=table(SchBGS[,14]),notch=T,main="Mean Schumaker score by MSPA class",ylab="Schumaker score",xlab="MSPA")

###############################################################################
#Do NCPA by distance to edge
##############################################################################

#Calculate Poisson change point
SchumakerCPA<-cp.pois(SchBGS$dist2edge,Schumaker[,2],data=as.data.frame(cbind(SchBGS$dist2edge,Schumaker[,2])))

#Make scatterplot
plot(SchumakerCPA$xx,SchumakerCPA$yy,xlab="Distance to edge",ylab="Mean Schumaker score",main="Poisson change point Schumaker score")

#Draw rectangle and change point line
rect(xleft=quantile(SchumakerCPA$bootrep[,1],0.1),xright=quantile(SchumakerCPA$bootrep[,1],0.9),ybottom=0,ytop=400,density=50)
abline(v=SchumakerCPA$all.out[1],col="red",lwd=2)

#Print quantiles
quantile(SchumakerCPA$bootrep[,1],0.1)
quantile(SchumakerCPA$bootrep[,1],0.9)
SchumakerCPA$all.out
