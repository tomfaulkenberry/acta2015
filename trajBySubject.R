# routine to compute earliest time step on which significant differences occur between x-coordinates of trajectories
# separate analyses for COMPONENTS and DISTANCES

# large

rawData<-read.table("~/Dropbox/experiments/MouseTrackFractions2Data/dataLarger.csv",sep=",",header=TRUE)

# clean up data
dataStep3<-subset(rawData,subset=error!=1) # remove errors

meanRT<-mean(dataStep3$RT)
sdRT<-sd(dataStep3$RT)
data<-subset(dataStep3,subset=RT<meanRT+3*sdRT & RT>meanRT-3*sdRT) # remove 3 SD outliers
attach(data)

dataLargeSmallComp<-subset(data,stimulus=="23"|stimulus=="34")
dataLargeLargeComp<-subset(data,stimulus=="69"|stimulus=="68")

dataLargeClose<-subset(data,stimulus=="23"|stimulus=="69")
dataLargeFar<-subset(data,stimulus=="34"|stimulus=="68")



# compute sig differences in x-coordinates of trajectories by subject by COMPONENTS

for (i in 19:28){  # do 1:16 first, then 19:28
  subjectLargeComp<-subset(dataLargeLargeComp,subset=dataLargeLargeComp$partNum==i)
  subjectSmallComp<-subset(dataLargeSmallComp,subset=dataLargeSmallComp$partNum==i)
  for (j in 26:126){
    test=t.test(subjectSmallComp[,j],subjectLargeComp[,j])
    cat(sprintf('Subject %i, X_%i, p=%f \n',i,j-25,test$p.value),file="~/Dropbox/experiments/MouseTrackFractions2Data/pValuesBySubject.txt",append=TRUE)
      
  }
}

positionsComp=c(77,38,14,31,12,37,47,34,75,45,43,51,46,8,49,40,41,33,3,32,35,41,44,51,50,61)
mean(positionsComp)


# compute sig differences in x-coordinates of trajectories by subject by DISTANCE

for (i in 19:28){  # do 1:16 first, then 19:28
  subjectFar<-subset(dataLargeFar,subset=dataLargeFar$partNum==i)
  subjectClose<-subset(dataLargeClose,subset=dataLargeClose$partNum==i)
  for (j in 26:126){
    test=t.test(subjectFar[,j],subjectClose[,j])
    cat(sprintf('Subject %i, X_%i, p=%f \n',i,j-25,test$p.value),file="~/Dropbox/experiments/MouseTrackFractions2Data/pValuesBySubject.txt",append=TRUE)
    
  }
}

positionsDist=c(94,39,15,28,80,56,48,49,82,21,54,66,37,63,74,39,67,38,56,21,75,86,56,71,63,64)
mean(positionsDist)

t.test(positionsComp,positionsDist,paired=TRUE)



# small fractions

rawData<-read.table("~/Dropbox/experiments/MouseTrackFractions2Data/dataSmaller.csv",sep=",",header=TRUE)

# clean up data
dataStep3<-subset(rawData,subset=error!=1) # remove errors

meanRT<-mean(dataStep3$RT)
sdRT<-sd(dataStep3$RT)
data<-subset(dataStep3,subset=RT<meanRT+3*sdRT & RT>meanRT-3*sdRT) # remove 3 SD outliers
attach(data)

dataSmallSmallComp<-subset(data,stimulus=="13"|stimulus=="14")
dataSmallLargeComp<-subset(data,stimulus=="39"|stimulus=="28")

dataSmallClose<-subset(data,stimulus=="13"|stimulus=="39")
dataSmallFar<-subset(data,stimulus=="14"|stimulus=="28")



# compute sig differences in x-coordinates of trajectories by subject by COMPONENTS

for (i in 19:28){  # do 1:16 first, then 19:28
  subjectLargeComp<-subset(dataSmallLargeComp,subset=dataSmallLargeComp$partNum==i)
  subjectSmallComp<-subset(dataSmallSmallComp,subset=dataSmallSmallComp$partNum==i)
  for (j in 26:126){
    test=t.test(subjectSmallComp[,j],subjectLargeComp[,j])
    cat(sprintf('Subject %i, X_%i, p=%f \n',i,j-25,test$p.value),file="~/Dropbox/experiments/MouseTrackFractions2Data/pValuesSmall.txt",append=TRUE)
    
  }
}

positionsComp=c(66,36,49,70,37,28,42,5,37,54,46,35,53,47,14,39,32,38,43,45,50,34,7,45,23,10)
mean(positionsComp)


# compute sig differences in x-coordinates of trajectories by subject by DISTANCE

for (i in 19:28){  # do 1:16 first, then 19:28
  subjectFar<-subset(dataSmallFar,subset=dataSmallFar$partNum==i)
  subjectClose<-subset(dataSmallClose,subset=dataSmallClose$partNum==i)
  for (j in 26:126){
    test=t.test(subjectFar[,j],subjectClose[,j])
    cat(sprintf('Subject %i, X_%i, p=%f \n',i,j-25,test$p.value),file="~/Dropbox/experiments/MouseTrackFractions2Data/pValuesSmall.txt",append=TRUE)
    
  }
}

positionsDist=c(81,59,59,38,59,71,58,69,84,70,60,32,59,44,27,81,13,68,72,59,86,57,72,59,48,59)
mean(positionsDist)

t.test(positionsComp,positionsDist,paired=TRUE)

