########## Measure trends in Cath volume
set.seed(123)
setwd("/Users/dscheink/Documents/MIT-MGH/EP_CathLab/R_PythonAnalysis/Joint Unit Model/ReModifiedAlgorithm/")

DF = read.csv("/Users/dscheink/Documents/MIT-MGH/EP_Cath/Raw Data/CathProcs03_14.csv")
CathSep = read.csv("JanSep14CathTimes.csv")
CathProcInfo = read.csv("JanJun14CathProcs.csv")

DF$Year = format(as.POSIXct(DF$Date_of_Cath, format="%m/%d/%y"),"%Y")
DF$Month = format(as.POSIXct(DF$Date_of_Cath, format="%m/%d/%y"),"%m")

#Example of calculation for 2013 monthly volume
CY2013 = DF[DF$Year == "2013",]
ProcsByMonth13 = as.data.frame.matrix(table(CY2013$Proc_Group, CY2013$Month))
ProcsByMonth13 = rbind(ProcsByMonth13,colSums(ProcsByMonth13))
row.names(ProcsByMonth13)[19] = "Total"

#Calculate data for all years
ProcsByYear = as.data.frame.matrix(table(DF$Proc_Group, DF$Year))
CathSep14 = as.data.frame(table(CathSep$Proc_Group))
ProcsByYear14 = cbind(ProcsByYear[c(1:9,11:14,16:18),1:14],CathSep14)
ProcsByYear14$Var1 = NULL
colnames(ProcsByYear14)[15]="JanSep2014"
ProcsByYear14$Estimate2014 = 4/3*ProcsByYear14$JanSep2014


write.csv(ProcsByYear14, "CathProcsYearly.csv")

write.csv(ProcsByYear, "CathProcsByYear.csv")

#Run a regression for each row and add the regression variable to the end of the DF for the years 2010-2014

data = ProcsByYear14
design.mat <- cbind(1,1:5)
response.mat <- t(data[,c(11:14,16)])

reg <- lm.fit(design.mat, response.mat)$coefficients
data <- cbind(data, t(reg))
colnames(data)[17:18]=c("intercept", "slope")
data$ChangeAsPercentageOf2014Estimate = data$slope/data$Estimate2014*100
data$ChangeNormalized = data$ChangeAsPercentageOf2014Estimate/abs(sum(data$ChangeAsPercentageOf2014Estimate[1:16]))
data = rbind(data,colSums(data))
row.names(data)[17] = "Total"



########## Use the trends in Cath volume to make various projections of volume growth

# Read in the relevant information
Volumes = read.csv("CathAndEPFull.csv")
#write.csv(Volumes, "CathAndEPProcSummaryRestricted.csv")

##########Based on the analysis above, we model various levels of growth and decline in each type of Cath procedure.###########

#This column of random numbers will let us add and remove procedures
Volumes$RandomNumber = 100*runif(length(Volumes[,1]))
Volumes$ID = 1:length(Volumes[,1])

###Subset the Cath data into growth and decline
Growth = subset(data, slope>0)
Decline = subset(data, slope<0)

###Make a vector to drop the appropriate percentage of each procedures for which the change is negative
#Set the number of total procedures to drop per year
#The default drop is 200 procedures for one year. This will correspond to about 100 procedures for the 6 month period considered

CathDrop = function(DF= Volumes, Growth = Growth, Decline = Decline, TotalDropPerYear = 200){

DropList = list()
for (i in 1:length(Decline[,1])){
  DropSubset = subset(Volumes, Volumes$ProcCat == row.names(Decline)[i])
  DropList[[i]] = DropSubset$ID[DropSubset$RandomNumber> 100 + (TotalDropPerYear/2)*Decline$ChangeNormalized[i]]
}

DropID = unlist(DropList)

###Make a vector to add the appropriate percentage of each procedures for which the change is positive

AddList = list()
for (i in 1:length(Growth[,1])){
  AddSubset = subset(Volumes, Volumes$ProcCat == row.names(Growth)[i])
  AddList[[i]] = AddSubset$ID[AddSubset$RandomNumber> 100 - (TotalDropPerYear/2)*Growth$ChangeNormalized[i]]
}

AddID = unlist(AddList)

#Cheack that this gives the desired total net change
length(DropID) - length(AddID)

VolumesCathDecrease = subset(Volumes, !(ID %in% DropID))

#Add the selected procedures
AddProcs = Volumes[Volumes$ID %in% AddID,]
if (length(AddProcs[,1])>0){
  AddProcs$ScheduleHorizon = 3
}

CathDecreaseIncrease = rbind(VolumesCathDecrease, AddProcs)

#Change the schedule horizon of the added procedures so that they can be performed at any time in a given week


return(CathDecreaseIncrease)
}


##########Based on potential for EP growth, model one and two more additional providers such as Heist Edwin K, MD, Phd########

View(table(as.character(Volumes$Attending[Volumes$Lab == "1"])))

View(table(as.character(Volumes$ProcCat[Volumes$Lab==1 & Volumes$Attending=="Heist Edwin K, MD, Phd"])))

AddEPProvider = function(Volumes = CathVolChange0, NumProviders = 1){

ExtraProvider = subset(Volumes, Attending=="Heist Edwin K, MD, Phd")
ExtraProvider$Attending = "New EP Provider 1"
ExtraProvider$ScheduleHorizon = 3

ExtraProvider2 = ExtraProvider
ExtraProvider2$Attending = "New EP Provider 2"

VolumeEPIncrease = rbind(Volumes, ExtraProvider)  

if (NumProviders == 2){
  VolumeEPIncrease = rbind(VolumeEPIncrease, ExtraProvider2)
}

return(VolumeEPIncrease)

}

##########Based on potential for EP growth, model one and two more additional providers such as Moussa########


##########Combine the Cath volume drops and EP voluem growth


CathVolChange0 = CathDrop(DF= Volumes, Growth = Growth, Decline = Decline, TotalDropPerYear = 0)
CathVolChange1 = CathDrop(DF= Volumes, Growth = Growth, Decline = Decline, TotalDropPerYear = 200)
CathVolChange2 = CathDrop(DF= Volumes, Growth = Growth, Decline = Decline, TotalDropPerYear = 400)

CathFlatEPFlat = CathVolChange0
CathFlatEPGrow1 = AddEPProvider(Volumes = CathVolChange0, NumProviders = 1)
CathFlatEPGrow2 = AddEPProvider(Volumes = CathVolChange0, NumProviders = 2)

CathDrop1EPFlat = CathVolChange1
CathDrop1EPGrow1 = AddEPProvider(Volumes = CathVolChange1, NumProviders = 1)
CathDrop1EPGrow2 = AddEPProvider(Volumes = CathVolChange1, NumProviders = 2)

CathDrop2EPFlat = CathVolChange2
CathDrop2EPGrow1 = AddEPProvider(Volumes = CathVolChange2, NumProviders = 1)
CathDrop2EPGrow2 = AddEPProvider(Volumes = CathVolChange2, NumProviders = 2)

#Remove the columns that won't be used for the Python script
ForPython = function(df){
  
return( df[,c("DayOfPeriod","WeekOfPeriod", "Lab", "RoomTimePlusTOMinutes", "ScheduleHorizon",
                                 "PossibleRooms", "PreProcTime", "PostProcTime","ProcKey", "AttendingKey")])
}

write.table(ForPython(CathFlatEPFlat), file='CathFlatEPFlat.csv', row.names=FALSE, col.names=FALSE,sep=",")
write.table(ForPython(CathFlatEPGrow1), file='CathFlatEPGrow1.csv', row.names=FALSE, col.names=FALSE,sep=",")
write.table(ForPython(CathFlatEPGrow2), file='CathFlatEPGrow2.csv', row.names=FALSE, col.names=FALSE,sep=",")

write.table(ForPython(CathDrop1EPFlat), file='CathDrop1EPFlat.csv', row.names=FALSE, col.names=FALSE,sep=",")
write.table(ForPython(CathDrop1EPGrow1), file='CathDrop1EPGrow1.csv', row.names=FALSE, col.names=FALSE,sep=",")
write.table(ForPython(CathDrop1EPGrow2), file='CathDrop1EPGrow2.csv', row.names=FALSE, col.names=FALSE,sep=",")

write.table(ForPython(CathDrop2EPFlat), file='CathDrop2EPFlat.csv', row.names=FALSE, col.names=FALSE,sep=",")
write.table(ForPython(CathDrop2EPGrow1), file='CathDrop2EPGrow1.csv', row.names=FALSE, col.names=FALSE,sep=",")
write.table(ForPython(CathDrop2EPGrow2), file='CathDrop2EPGrow2.csv', row.names=FALSE, col.names=FALSE,sep=",")

VolStats = 
  data.frame(rbind(summary(CathFlatEPFlat$RoomTimePlusTOMinutes),
                   summary(CathFlatEPGrow1$RoomTimePlusTOMinutes),
                   summary(CathFlatEPGrow2$RoomTimePlusTOMinutes),
                   summary(CathDrop1EPFlat$RoomTimePlusTOMinutes),
                   summary(CathDrop1EPGrow1$RoomTimePlusTOMinutes),
                   summary(CathDrop1EPGrow2$RoomTimePlusTOMinutes),
                   summary(CathDrop2EPFlat$RoomTimePlusTOMinutes),
                   summary(CathDrop2EPGrow1$RoomTimePlusTOMinutes),
                   summary(CathDrop2EPGrow2$RoomTimePlusTOMinutes)
                   ))

CathFlatEPFlat$Scenario = "CathFlatEPFlat"
CathFlatEPGrow1$Scenario = "CathFlatEPGrow1"
CathFlatEPGrow2$Scenario = "CathFlatEPGrow2"
CathDrop1EPFlat$Scenario = "CathDrop1EPFlat"
CathDrop1EPGrow1$Scenario = "CathDrop1EPGrow1"
CathDrop1EPGrow2$Scenario = "CathDrop1EPGrow2"
CathDrop2EPFlat$Scenario = "CathDrop2EPFlat"
CathDrop2EPGrow1$Scenario = "CathDrop2EPGrow1"
CathDrop2EPGrow2$Scenario = "CathDrop2EPGrow2"

AllScenarios = rbind(
  CathFlatEPFlat, 
  CathFlatEPGrow1, 
  CathFlatEPGrow2, 
  CathDrop1EPFlat, 
  CathDrop1EPGrow1, 
  CathDrop1EPGrow2, 
  CathDrop2EPFlat,
  CathDrop2EPGrow1,
  CathDrop2EPGrow2)

DT = as.data.table(AllScenarios)

Scenarios = as.data.frame(DT[, list(length(RoomTimePlusTOMinutes), mean(RoomTimePlusTOMinutes), median(RoomTimePlusTOMinutes) ), by = Scenario])

write.csv(Scenarios, "CathEPVolumeScenarios.csv")
