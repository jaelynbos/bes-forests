################################################################################
#Depth of organic matter
#Jaelyn Bos
################################################################################

################################################################################
#Required packages: ggplot2, stringr, dplyr
###############################################################################

library(ggplot2)
library(stringr)
library(dplyr)

#Soil is defined as organic matter when chroma <6 and value <4

##############################################################################
#Load pre-2018 data
##############################################################################

setwd("C:/Users/Jaelyn Bos/Documents/Baltimore Forestry data/2018")
SoilO2018<-read.csv("Soil Punch Data.csv")

#Pull out top soil layer
SoilO2018<-SoilO2018[SoilO2018$Layer.Number==1,]

#Remove NAs
SoilO2018<-SoilO2018[!(SoilO2018[,5]==9999999),]
SoilO2018<-SoilO2018[!(SoilO2018[,9]==9999999),]
SoilO2018<-SoilO2018[!is.na(SoilO2018[,5]),]

#Define samples as organic matter, or not
SoilO2018[(SoilO2018$Value>4),5]<-0
SoilO2018[(SoilO2018$Chroma>6),5]<-0

#Calculate depth of OM
O2018<-SoilO2018[,5]-SoilO2018[,4]

SoilO2018.1<-cbind(SoilO2018,O2018)
colnames(SoilO2018.1)<-c(colnames(SoilO2018),"O")

#Load environment data
setwd("C:/Users/Jaelyn Bos/Documents/Baltimore Forestry data")
BGS2020<-read.csv("BGS_coords2020_attributes.csv")

#Remove duplicate rows
BGS2020<-BGS2020[!duplicated(BGS2020[,2]),]

#Left_join soil data and environmnetal data
O2017BGS<-left_join(as.data.frame(SoilO2018.1),as.data.frame(BGS2020),by=c( "Point.Code" = "Plot_Name" ))

##############################################################################
#Load 2018 data
##############################################################################

setwd("C:/Users/Jaelyn Bos/Documents/Baltimore Forestry data/2018")
SoilO2018B<-read.csv("SoilPunch_2018B.csv")

#This dataset alreay had depthof organic matter calculated, so we 
#only have to extract the appropriate column

#Remove NAs
SoilO2018B<-SoilO2018B[!SoilO2018B$Depth..O=="",]

#Reformat character data as numeric
SoilO2018B$Depth..O<-as.numeric(trimws(unlist(strsplit(as.character(SoilO2018B$Depth..O)," cm"))))

#Remove NAs again
SoilO2018B<-SoilO2018B[!is.na(SoilO2018B$Depth..O),]
O2018B<-SoilO2018B$Depth..O
SoilO2018B.1<-cbind(SoilO2018B,O2018B)
colnames(SoilO2018B.1)<-c(colnames(SoilO2018B),"O")

#Left_join soil data and environmnetal data
O2018BGS<-left_join(as.data.frame(SoilO2018B.1),as.data.frame(BGS2020),by=c( "Plot.Name..Pt..." = "Plot_Name" ))


#############################################################################
#Load 2019 data
#############################################################################
setwd("C:/Users/Jaelyn Bos/Documents/Baltimore Forestry data/2019")
SoilO2019<-read.csv("SoilPunch2019.csv")

#Specify top layer
SoilO2019<-SoilO2019[SoilO2019$Layer.Number==1,]

#Recode factors as numeric
SoilO2019[,5]<-as.numeric(as.character(SoilO2019[,5]))
SoilO2019[,4]<-as.numeric(as.character(SoilO2019[,4]))

#Remove NAs
SoilO2019<-SoilO2019[!is.na(SoilO2019[,4]),]
SoilO2019<-SoilO2019[!is.na(SoilO2019[,5]),]
SoilO2019<-SoilO2019[!is.na(SoilO2019[,9]),]
SoilO2019<-SoilO2019[!is.na(as.numeric(as.character(SoilO2019[,8]))),]

#Identify OM by value and chroma
SoilO2019[as.numeric(as.character(SoilO2019$Value))>4,5]<-0
SoilO2019[as.numeric(as.character(SoilO2019$Chroma))>6,5]<-0

O2019<-SoilO2019[,5]
SoilO2019.1<-cbind(SoilO2019,O2019)
colnames(SoilO2019.1)<-c(colnames(SoilO2019),"O")

#Left_join soil data and environmnetal data
O2019BGS<-left_join(as.data.frame(SoilO2019.1),as.data.frame(BGS2020),by=c( "Point.Code" = "Plot_Name" ))


##########################################################################
#Combine all years
##########################################################################

#Select point code, depth of OM, dist2edge, and MSPA class
O2017BGS<-O2017BGS[,c(2,10,18,22)]
O2018BGS<-O2018BGS[,c(1,28,36,40)]
O2019BGS<-O2019BGS[,c(2,10,18,22)]

#Rename columns to standardize
colnames(O2018BGS)<-c("Point.Code","O","dist2edge","mspa")

#Combine all
OBGS<-rbind(O2017BGS,O2018BGS,O2019BGS)

#Recode MSPA class
OBGS$mspa<-as.factor(OBGS$mspa)
levels(OBGS$mspa)<-c("other","connected canopy","edge","other","core","connected canopy","edge","connected canopy","edge","core")

#Make boxplot
boxplot(OBGS$O~OBGS$mspa,ylim=c(0,300),notch=T,ylab="Depth of organic matter",xlab="MSPA class",main="Depth of OM by MSPA classs")

#############################################################################
#Make bar chart of %OM for analyzed samples
#############################################################################

#Load data
setwd("C:/Users/Jaelyn Bos/Documents/Baltimore forestry data/Pre2018")
OMatter<-read.csv("Organic Matter.csv")
colnames(OMatter)<-c("PointCode","SampleNumber","OrganicMatter","ID")

#Extract site name from point code
Site<-str_split(as.character(OMatter$PointCode)," ")
Site<-unlist(lapply(Site,function(x){return(x[1])}))

#Make data frame of relevant data
df<-as.data.frame(cbind(as.character(Site),as.numeric(OMatter$OrganicMatter)))

#Remove NAs
df<-df[!is.na(df[,2]),]

#Rename columns
colnames(df)<-c("Site","O")

#Convert factor to numeric
df[,2]<-as.numeric(as.character(df[,2]))

#Get standard deviation of each site for error bars
sds<-aggregate(df[,2],list(df[,1]),sd)
l<-aggregate(df[,2],list(df[,1]),length)
l<-sqrt(l[,2])
sem<-cbind(sds,l)
sem<-sem[,2]/sem[,3]

#Get means of sites for chart
df<-aggregate(df[,2],list(df[,1]),mean)
df<-cbind(df,sem)
colnames(df)<-c("Site","O","sem")

#Make plot
ggplot(df,aes(x=Site,y=O))+geom_col()+geom_errorbar(aes(ymin=O-sem, ymax=O+sem), width=.1)+labs(title="% organic matter in soil",y="% OM")