rawData<-read.table("data.csv",sep=",",header=TRUE)

# clean up data
dataStep3<-subset(rawData,subset=error!=1) # remove errors

meanRT<-mean(dataStep3$RT)
sdRT<-sd(dataStep3$RT)
data<-subset(dataStep3,subset=RT<meanRT+3*sdRT & RT>meanRT-3*sdRT) # remove 3 SD outliers
attach(data)


# produce trajectory graphs

dataLargeSmallComp<-subset(data,compSize=="small" & decision=="larger")
dataLargeLargeComp<-subset(data,compSize=="large" & decision=="larger")
dataSmallSmallComp<-subset(data,compSize=="small" & decision=="smaller")
dataSmallLargeComp<-subset(data,compSize=="large" & decision=="smaller")

dataLargeClose<-subset(data,distance=="small" & decision=="larger")
dataLargeFar<-subset(data,distance=="large" & decision=="larger")
dataSmallClose<-subset(data,distance=="small" & decision=="smaller")
dataSmallFar<-subset(data,distance=="large" & decision=="smaller")

xCoords=rep(0,808)
yCoords=rep(0,808)
manip=rep(0,808)
size=rep(0,808)
decision=rep(0,808)

for (i in 1:101){
  xCoords[i]=mean(dataLargeSmallComp[,i+28])
  yCoords[i]=mean(dataLargeSmallComp[,i+129])
  manip[i]="by component size"
  size[i]="small"
  decision[i]="larger than 1/2"
  
  xCoords[i+101]=mean(dataLargeLargeComp[,i+28])
  yCoords[i+101]=mean(dataLargeLargeComp[,i+129])
  manip[i+101]="by component size"
  size[i+101]="large"
  decision[i+101]="larger than 1/2"
  
  xCoords[i+202]=mean(dataSmallSmallComp[,i+28])
  yCoords[i+202]=mean(dataSmallSmallComp[,i+129])
  manip[i+202]="by component size"
  size[i+202]="small"
  decision[i+202]="smaller than 1/2"
  
  xCoords[i+303]=mean(dataSmallLargeComp[,i+28])
  yCoords[i+303]=mean(dataSmallLargeComp[,i+129])
  manip[i+303]="by component size"
  size[i+303]="large"
  decision[i+303]="smaller than 1/2"
  
    
  xCoords[i+404]=mean(dataLargeClose[,i+28])
  yCoords[i+404]=mean(dataLargeClose[,i+129])
  manip[i+404]="by distance"
  size[i+404]="small"
  decision[i+404]="larger than 1/2"
    
  xCoords[i+505]=mean(dataLargeFar[,i+28])
  yCoords[i+505]=mean(dataLargeFar[,i+129])
  manip[i+505]="by distance"
  size[i+505]="large"
  decision[i+505]="larger than 1/2"
  
  xCoords[i+606]=mean(dataSmallClose[,i+28])
  yCoords[i+606]=mean(dataSmallClose[,i+129])
  manip[i+606]="by distance"
  size[i+606]="small"
  decision[i+606]="smaller than 1/2"
  
  xCoords[i+707]=-mean(dataLargeFar[,i+28])
  yCoords[i+707]=mean(dataLargeFar[,i+129])
  manip[i+707]="by distance"
  size[i+707]="large"
  decision[i+707]="smaller than 1/2"
}

library("ggplot2")
trajectoryData=data.frame(xCoords,yCoords,manip,size,decision)
plot=ggplot(trajectoryData,aes(x=xCoords,y=yCoords,group=size))+xlim(-1,1)+ylim(0,1.5)
paths=geom_path(aes(linetype=size),size=1.3)
colors=scale_colour_manual(values=c("blue","red"))
labels=labs(x="x-coordinates",y="y-coordinates")
faceting=facet_grid(decision~manip)
stripFormat=theme(strip.text=element_text(face="bold",size=rel(1.5)))
legendFormat=theme(legend.title=element_text(face="bold",size=rel(1.5)),legend.text=element_text(size=rel(1.5)))
axesFormat=theme(axis.title=element_text(size=rel(1.4)))

basePlot=plot+paths+colors+labels+faceting+stripFormat+legendFormat+axesFormat
basePlot+labs(colour="Size")+theme(legend.position=c(0.9,0.15))

# note:  export image as 600x600 to get same dimensions as Acta Psychologica article


# PERFORMANCE MEASURES
# RT
# manip=by compSize
agg=aggregate(RT~subject+compSize+decision,data=data,FUN="mean") # RT performance data aggregated by subject
RT.aov=aov(RT~as.factor(compSize)*as.factor(decision)+Error(as.factor(subject)/(as.factor(compSize)*as.factor(decision))),data=agg)
summary(RT.aov)
print(model.tables(RT.aov,"means"),digits=6)
sd(agg$RT[agg$compSize=="large" & agg$decision=="smaller"]) # edit this to get various SDs

# RT
# manip=by Distance
agg=aggregate(RT~subject+distance+decision,data=data,FUN="mean") # RT performance data aggregated by subject
RT.aov=aov(RT~as.factor(distance)*as.factor(decision)+Error(as.factor(subject)/(as.factor(distance)*as.factor(decision))),data=agg)
summary(RT.aov)
print(model.tables(RT.aov,"means"),digits=6)
sd(agg$RT[agg$distance=="large" & agg$decision=="smaller"]) # edit this to get various SDs


# init
# manip=by compSize
agg=aggregate(init~subject+compSize+decision,data=data,FUN="mean") # RT performance data aggregated by subject
init.aov=aov(init~as.factor(compSize)*as.factor(decision)+Error(as.factor(subject)/(as.factor(compSize)*as.factor(decision))),data=agg)
summary(init.aov)
print(model.tables(init.aov,"means"),digits=6)
sd(agg$init[agg$compSize=="large" & agg$decision=="smaller"]) # edit this to get various SDs

# init
# manip=by Distance
agg=aggregate(init~subject+distance+decision,data=data,FUN="mean") # RT performance data aggregated by subject
init.aov=aov(init~as.factor(distance)*as.factor(decision)+Error(as.factor(subject)/(as.factor(distance)*as.factor(decision))),data=agg)
summary(init.aov)
print(model.tables(init.aov,"means"),digits=6)
sd(agg$init[agg$distance=="large" & agg$decision=="smaller"]) # edit this to get various SDs

# MD
# manip=by compSize
agg=aggregate(MD~subject+compSize+decision,data=data,FUN="mean") # RT performance data aggregated by subject
MD.aov=aov(MD~as.factor(compSize)*as.factor(decision)+Error(as.factor(subject)/(as.factor(compSize)*as.factor(decision))),data=agg)
summary(MD.aov)
print(model.tables(MD.aov,"means"),digits=6)
sd(agg$MD[agg$compSize=="large" & agg$decision=="smaller"]) # edit this to get various SDs

# MD
# manip=by Distance
agg=aggregate(MD~subject+distance+decision,data=data,FUN="mean") # RT performance data aggregated by subject
MD.aov=aov(MD~as.factor(distance)*as.factor(decision)+Error(as.factor(subject)/(as.factor(distance)*as.factor(decision))),data=agg)
summary(MD.aov)
print(model.tables(MD.aov,"means"),digits=6)
sd(agg$MD[agg$distance=="large" & agg$decision=="smaller"]) # edit this to get various SDs

# AUC
# manip=by compSize
agg=aggregate(AUC~subject+compSize+decision,data=data,FUN="mean") # RT performance data aggregated by subject
AUC.aov=aov(AUC~as.factor(compSize)*as.factor(decision)+Error(as.factor(subject)/(as.factor(compSize)*as.factor(decision))),data=agg)
summary(AUC.aov)
print(model.tables(AUC.aov,"means"),digits=6)
sd(agg$MD[agg$compSize=="large" & agg$decision=="smaller"]) # edit this to get various SDs

# AUC
# manip=by Distance
agg=aggregate(AUC~subject+distance+decision,data=data,FUN="mean") # RT performance data aggregated by subject
AUC.aov=aov(AUC~as.factor(distance)*as.factor(decision)+Error(as.factor(subject)/(as.factor(distance)*as.factor(decision))),data=agg)
summary(AUC.aov)
print(model.tables(AUC.aov,"means"),digits=6)
sd(agg$MD[agg$distance=="large" & agg$decision=="smaller"]) # edit this to get various SDs


# MD time
# manip=by compSize
agg=aggregate(MD_time~subject+compSize+decision,data=data,FUN="mean") # RT performance data aggregated by subject
MDtime.aov=aov(MD_time~as.factor(compSize)*as.factor(decision)+Error(as.factor(subject)/(as.factor(compSize)*as.factor(decision))),data=agg)
summary(MDtime.aov)
print(model.tables(MDtime.aov,"means"),digits=6)
sd(agg$MD[agg$compSize=="large" & agg$decision=="smaller"]) # edit this to get various SDs

# MD time
# manip=by Distance
agg=aggregate(MD_time~subject+distance+decision,data=data,FUN="mean") # RT performance data aggregated by subject
MDtime.aov=aov(MD_time~as.factor(distance)*as.factor(decision)+Error(as.factor(subject)/(as.factor(distance)*as.factor(decision))),data=agg)
summary(MDtime.aov)
print(model.tables(MDtime.aov,"means"),digits=6)
sd(agg$MD[agg$distance=="large" & agg$decision=="smaller"]) # edit this to get various SDs


# analysis of when deviations happen by subject
# see file trajBySubject.R for script to calculate by subject

positionsCompLarge=c(77,38,14,31,12,37,47,34,75,45,43,51,46,8,49,40,41,33,3,32,35,41,44,51,50,61)
positionsDistLarge=c(94,39,15,28,80,56,48,49,82,21,54,66,37,63,74,39,67,38,56,21,75,86,56,71,63,64)
positionsCompSmall=c(66,36,49,70,37,28,42,5,37,54,46,35,53,47,14,39,32,38,43,45,50,34,7,45,23,10)
positionsDistSmall=c(81,59,59,38,59,71,58,69,84,70,60,32,59,44,27,81,13,68,72,59,86,57,72,59,48,59)

subject=rep(1:26,4)
manip=rep(0,104)
decision=rep(0,104)
splitPosition=rep(0,104)

for (i in 1:26){
  manip[i]="by component"
  decision[i]="larger"
  splitPosition[i]=positionsCompLarge[i]
}

for (i in 27:52){
  manip[i]="by distance"
  decision[i]="larger"
  splitPosition[i]=positionsDistLarge[i-26]
}

for (i in 53:78){
  manip[i]="by component"
  decision[i]="smaller"
  splitPosition[i]=positionsCompSmall[i-52]
}

for (i in 79:104){
  manip[i]="by distance"
  decision[i]="smaller"
  splitPosition[i]=positionsDistSmall[i-78]
}

splitData=data.frame(subject,manip,decision,splitPosition)

split.aov=aov(splitPosition~manip*decision+Error(as.factor(subject)/(manip*decision)),data=splitData)
summary(split.aov)
print(model.tables(split.aov,"means"),digits=6)


# bimodality coefficients
MDvector=data$z.MD.separate

# histogram of MD values
library("ggplot2")

basePlot=ggplot(NULL,aes(x=MDvector))+geom_histogram(binwidth=0.4,fill="white",colour="black")
labels=labs(x="Maximum deviation (MD)",y="Frequency")
axesFormat=theme(axis.title=element_text(size=rel(1.4)))

basePlot+labels+axesFormat

# computations
# Hartigan's dip statistic

library("diptest")
dip.test(MDvector)

# Bimodality coefficient (SAS)
library("moments")

s=skewness(MDvector)
k=kurtosis(MDvector)
n=length(MDvector)

BC=(s^2+1)/(k+(3*(n-1)^2)/((n-2)*(n-3)))
BC
