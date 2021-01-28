##############################################################################
#Penetrometer boxplot
#and Gaussian nCPA
#Jaelyn Bos
##############################################################################

##############################################################################
#Prior to running this script, run "Taxa and environmental data.R"
#Required packages: dplyr
#############################################################################

library(dplyr)

#Get pre-2018 penetrometer data
setwd("C:/Users/Jaelyn Bos/Documents/Baltimore Forestry data/Pre2018")
Pen2017<-read.csv("Penetrometer.csv")
Pen2017<-Pen2017[,c(4,3)]

#Get 2018 penetrometer data
setwd("C:/Users/Jaelyn Bos/Documents/Baltimore Forestry data/2018")
Pen2018<-read.csv("PrismSoils_2018B.csv")
Pen2018<-Pen2018[,c(1,8)]

#Get 2019 penetrometer data
setwd("C:/Users/Jaelyn Bos/Documents/Baltimore Forestry data/2019")
Pen2019<-read.csv("Penetrometer 2019.csv")
Pen2019<-Pen2019[,c(2,4)]

#Combine data from all years
PenPSI<-c(as.numeric(as.character(Pen2017[,2])),as.numeric(as.character(Pen2018[,2])),as.numeric(as.character(Pen2019[,2])))
PenPoints<-(c(as.character(Pen2017[,1]),as.character(Pen2018[,1]),as.character(Pen2019[,1])))

Pen<-as.data.frame(cbind(PenPoints,PenPSI))

#Remove NAs
Pen<-Pen[!is.na(Pen[,2]),]

#Aggregate points with multiple readings
Pen<-aggregate(as.numeric(as.character(Pen[,2])),list(trimws(as.character(Pen[,1]))),mean)

#Replace missnamed points
Pen[,1]<-str_replace(Pen[,1],"Glen Plot 2/ Pt.","Glen Plot 2 Pt.")
Pen[,1]<-str_replace(Pen[,1],"Glen Plot 3/ Pt.","Glen Plot 3 Pt.")
Pen[,1]<-str_replace(Pen[,1],"HURUN","HERUN")

colnames(Pen)<-c("Plot_Name","MeanPSI")

#Combine with other environmental data
PenBGS<-left_join(Pen,BGS2020)

#Remove NA's
PenBGS<-PenBGS[!is.na(PenBGS$mspa),]
PenBGS<-PenBGS[!(PenBGS$mspa==-9999),]

#Make MSPA factor and recode
PenBGS$mspa<-as.factor(PenBGS$mspa)
levels(PenBGS$mspa)<-c("other","connected canopy","edge","other","core","connected canopy","edge","connected canopy","edge","core")

#Make the plot
boxplot(PenBGS[,2]~PenBGS[,14],notch=T,xlab="MSPA class",width=table(PenBGS[,14]),ylab="Penetrometer reading",main="Penetrometer readings by MSPA class")

##############################################################################
#On to nCPA
##############################################################################

#Calculate Gaussian change point
PenCPA<-cp.gauss(PenBGS$dist2edge,PenBGS$MeanPSI,data=as.data.frame(cbind(PenBGS$dist2edge,PenBGS$MeanPSI)))

#Make scatterplot
plot(PenCPA$xx,PenCPA$yy,xlab="Distance to edge",ylab="Penetrometer reading",main="Gaussian change point penetrometer reading")

#Draw rectangle, and red line for change point
rect(xleft=quantile(PenCPA$bootrep[,1],0.1),xright=quantile(PenCPA$bootrep[,1],0.9),ybottom=0,ytop=400,density=50)
abline(v=PenCPA$all.out[1],col="red",lwd=2)

#Print quantiles
PenCPA$all.out[1]
quantile(PenCPA$bootrep[,1],0.1)
quantile(PenCPA$bootrep[,1],0.9)

