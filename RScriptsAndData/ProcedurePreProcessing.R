library(data.table)
set.seed(123)

"""
This script formats, manipulates, and combines Cath and EP procedural data 
taken over a 25 week period to include the following information:

    For each case (procedure):
        1. The lab in which it took place (0 = Cath, 1 = EP)
        2. The day of the period it took place (1-125)
        3. The week it took place (1-25)
        4. In room time (Cath - from timestamps; EP - from case start to end + padding from a normal distribution)
        5. Restrictions on procedure location (0 = Cath only, 1 = EP only, 2 = either)
        6. Scheduling Horizon

The output of this script is intended for use by a Python script to model
efficient scheduling for the combined labs.

Note: Drops all non-primetime procedures
"""

setwd("/Users/dscheink/Documents/MIT-MGH/EP_Cath/Git/mghSchedulingModel/RScriptsAndData")

################### CATH ######################

# read Cath times and procedure info
Cath = read.csv("JanJun14CathTimesProvider.csv")
CathProcInfo = read.csv("JanJun14CathProcs.csv")
# TAVR's that are scheduled to leave the Cath lab
TAVRid = CathProcInfo$SS_Event_Cath_ID[CathProcInfo$Cathegory == "Valve-TVT"]
Cath = subset(Cath, !(Cath$SS_Event_Cath_ID %in% TAVRid))

################### EP ######################

# read EP times and procedure info
EP = read.csv("JanJun14EPTimesProvider.csv")
EPProcInfo = read.csv("JanJun14EPProcs.csv")
# keep only procedures that happened in Lab1, Lab2 or Lab3
EP = subset(EP, ProcedureRoom %in% c("EPLAB1","EPLAB2","EPLAB3","Ellison9lab", "Middle Room"))

# add information about the labs that the procedures took place in
Cath$Lab = 0
EP$Lab = 1

################ DATE FORMATTING ################

# format dates to be readable by R and only keep usable subset of data
Cath$Date0am = as.POSIXlt(paste(Cath$Date_of_Cath, "00:00:01 AM", sep = " "),format = "%m/%d/%y %H:%M:%S")
Cath$Date8am = as.POSIXlt(paste(Cath$Date_of_Cath, "8:00:00 AM", sep = " "),format = "%m/%d/%y %r")
Cath$Date6pm = as.POSIXlt(paste(Cath$Date_of_Cath, "6:00:00 PM", sep = " "),format = "%m/%d/%y %r")
Cath$Date = as.Date(as.POSIXlt(Cath$Date_of_Cath, format = "%m/%d/%y"))
Cath$In.LabT = as.POSIXlt(paste(Cath$Date_of_Cath, Cath$Arrived.to.HR, sep = " "),format = "%m/%d/%y %r")
Cath$In.RoomT = as.POSIXlt(paste(Cath$Date_of_Cath, Cath$Patient_in_Room, sep = " "),format = "%m/%d/%y %r")
Cath$Out.RoomT = as.POSIXlt(paste(Cath$Date_of_Cath, Cath$PatientOutOfRoom, sep = " "),format = "%m/%d/%y %r")
#Cath = subset(Cath, !is.na(In.RoomT) & !is.na(Out.RoomT))
#Cath = subset(Cath, Cath$Out.RoomT > Cath$In.RoomT)

EP$Date0am = as.POSIXlt(paste(EP$EventDate, "00:00:01 AM", sep = " "),format = "%m/%d/%y %H:%M:%S")
EP$Date8am = as.POSIXlt(paste(EP$EventDate, "8:00:00 AM", sep = " "),format = "%m/%d/%y %r")
EP$Date6pm = as.POSIXlt(paste(EP$EventDate, "6:00:00 PM", sep = " "),format = "%m/%d/%y %r")
EP$Date = as.Date(as.POSIXlt(EP$EventDate, format = "%m/%d/%y"))
EP$CaseStart = as.POSIXlt(paste(EP$EventDate, EP$ProcTimeCaseStarted, sep = " "),format = "%m/%d/%y %r")
EP$CaseEnd = as.POSIXlt(paste(EP$EventDate, EP$ProcTimeCaseEnd, sep = " "),format = "%m/%d/%y %r")
#### TO DO: padding from a normal distribution ####
#EP$In.RoomT = as.POSIXlt(paste(EP$EventDate, EP$Patient_in_Room, sep = " "),format = "%m/%d/%y %r")
#EP$Out.RoomT = as.POSIXlt(paste(EP$EventDate, EP$PatientOutOfRoom, sep = " "),format = "%m/%d/%y %r")
#EP = subset(EP, !is.na(CaseStart) & !is.na(CaseEnd))
#EP = subset(EP, EP$CaseEnd > EP$CaseStart)

############### Fill in estimate proc lengths for procedures that are missing time stamps


# calculate case time for viable procedures CATH
viableProcsCath = subset(Cath, !is.na(In.RoomT) & !is.na(Out.RoomT) & !is.na(Cath$In.LabT))
viableProcsCath = subset(viableProcsCath, Out.RoomT > In.RoomT & In.RoomT >=In.LabT)
viableProcsCath$RoomTime = difftime(viableProcsCath$Out.RoomT,viableProcsCath$In.RoomT, unit = "min")
viableProcsCath$RoomTimeMinutes = as.numeric(viableProcsCath$RoomTime)
#Add 35 for turnover time
viableProcsCath$RoomTimePlusTOMinutes = viableProcsCath$RoomTimeMinutes + 
  pmin(viableProcsCath$RoomTimeMinutes,rnorm(length(viableProcsCath$RoomTimeMinutes),35,5))
#Calculate the pre room time
viableProcsCath$PreProc = difftime(viableProcsCath$In.RoomT, viableProcsCath$In.LabT, unit= "min")
# use for manipulations
viableProcsCath$PreProcTime = as.numeric(viableProcsCath$PreProc)



# isolate the non-viable procedures
nonviableProcsCath = subset(Cath,   is.na(In.RoomT) | 
                                    is.na(Out.RoomT) | 
                                    is.na(In.LabT) | 
                                    Out.RoomT < In.RoomT | 
                                    In.RoomT < In.LabT
                            )

#For each of the non-viable procedures use the means of the corresponding times of the viable ones of that procedure
nonviableProcsCath$RoomTime = 0
nonviableProcsCath$RoomTimeMinutes = 0
nonviableProcsCath$RoomTimePlusTOMinutes = 0
nonviableProcsCath$PreProc = 0
nonviableProcsCath$PreProcTime = 0



SampleFunction = function(i =1, ColName = "RoomTime", MatchColumn = "Proc_Group", NonViableDF = nonviableProcsCath,ViableDF = viableProcsCath){
  return(subset(ViableDF, Proc_Group == as.character(NonViableDF[i,MatchColumn]))[max(1,round(sum( ViableDF[ , MatchColumn] == NonViableDF[i,MatchColumn])*runif(1))),ColName])
}

for (i in 1:length(nonviableProcsCath[,1])){
  nonviableProcsCath[i,"RoomTime"] = SampleFunction(i,"RoomTime", "Proc_Group", nonviableProcsCath,viableProcsCath)
  nonviableProcsCath[i,"RoomTimeMinutes"] = SampleFunction(i,"RoomTimeMinutes", "Proc_Group", nonviableProcsCath,viableProcsCath)
  nonviableProcsCath[i,"RoomTimePlusTOMinutes"] = SampleFunction(i,"RoomTimePlusTOMinutes", "Proc_Group", nonviableProcsCath,viableProcsCath)
  nonviableProcsCath[i,"PreProc"] = SampleFunction(i,"PreProc", "Proc_Group", nonviableProcsCath,viableProcsCath)
  nonviableProcsCath[i,"PreProcTime"] = SampleFunction(i,"PreProcTime", "Proc_Group", nonviableProcsCath,viableProcsCath)
}


############## In room time calculations for MiddleRoom ############## 

# calculate case time for viable procedures in EP
viableProcsEP = subset(EP, !is.na(CaseStart) & !is.na(CaseEnd))
viableProcsEP = subset(viableProcsEP, CaseEnd > CaseStart)
viableProcsEP$CaseTime = difftime(viableProcsEP$CaseEnd,viableProcsEP$CaseStart, unit = "min")
viableProcsEP$CaseTimeMinutes = as.numeric(viableProcsEP$CaseTime)
# isolate the non-viable procedures
nonViableProcsEP = subset(EP,(is.na(CaseStart) | is.na(CaseEnd) | CaseEnd < CaseStart))

#############Add turnover time to procedures that have time stamps

## Non MiddleRoom procedures first
viableProcsEP$RoomTimePlusTOMinutes = 0 
NonMR = !(viableProcsEP$ProcedureRoom %in% "Middle Room")
viableProcsEP$RoomTimePlusTOMinutes[NonMR] = viableProcsEP$CaseTimeMinutes[NonMR] + pmin(viableProcsEP$CaseTimeMinutes[NonMR],rnorm(sum(NonMR),85,10))
viableProcsEP$RoomTimePlusTOMinutes[!NonMR] = viableProcsEP$CaseTimeMinutes[!NonMR]

#Count the number of procedures not in the middle room that are missing time stamps
length(subset(EP,!(ProcedureRoom %in% "Middle Room"), )[,1]) - 
  length(subset(EP, !is.na(CaseStart) & !is.na(CaseEnd) & !(ProcedureRoom %in% "Middle Room"), )[,1])

#Look at the procedure mix for procedures with missing time stamps
View(table(as.character(nonViableProcsEP$ProcedureTypeOfStudy)))
View(table(as.character(viableProcsEP$ProcedureTypeOfStudy)))

#Add times to the procedures that don't have them from the procedures that do have them
SampleFunction2 = function(i =1, ColName = "RoomTime", MatchColumn = "ProcedureTypeOfStudy", NonViableDF = nonViableProcsEP,ViableDF = viableProcsEP){
  return(subset(ViableDF, ProcedureTypeOfStudy == as.character(NonViableDF[i,MatchColumn]))[max(1,round(sum( ViableDF[ , MatchColumn] == NonViableDF[i,MatchColumn])*runif(1))),ColName])
}

nonViableProcsEP$CaseTime = 0
nonViableProcsEP$CaseTimeMinutes = 0
nonViableProcsEP$RoomTimePlusTOMinutes = 0
for (i in 1:length(nonViableProcsEP[,1])){
  nonViableProcsEP[i,"CaseTime"] = SampleFunction2(i,"CaseTime", "ProcedureTypeOfStudy", nonViableProcsEP,viableProcsEP)
  nonViableProcsEP[i,"CaseTimeMinutes"] = SampleFunction2(i,"CaseTimeMinutes", "ProcedureTypeOfStudy", nonViableProcsEP,viableProcsEP)
  nonViableProcsEP[i,"RoomTimePlusTOMinutes"] = SampleFunction2(i,"RoomTimePlusTOMinutes", "ProcedureTypeOfStudy", nonViableProcsEP,viableProcsEP)
}

#Drop the unlabeled non-viable procedure
nonViableProcsEP=subset(nonViableProcsEP, !ProcedureTypeOfStudy == "")


################ Merge the procedures################

CathOld = Cath
EPOld = EP


Cath = rbind(viableProcsCath,nonviableProcsCath)
EP = rbind(viableProcsEP, nonViableProcsEP)


################ CATH EARLY AND LATE PROCEDURES ################

Cath$WeekDay = weekdays(Cath$Date)
# lose pieces of a factor = convert to character
Cath$Date_of_Cath = as.character(Cath$Date_of_Cath)
Cath = Cath[Cath$WeekDay != "Saturday" & Cath$WeekDay != "Sunday",]
Cath$Room = as.character(Cath$Room)



#Mark those procedures that started outside the 8am-6pm prime time hours and mark emergency procedures
Cath$Pre8AMStartFlag = (Cath$Date8am > Cath$In.RoomT)
Cath$Post6PMEndFlag = (Cath$Date6pm < Cath$Out.RoomT)
Cath$EmergencyFlag = (Cath$Origin=="Emergency Room"
                      | Cath$Origin=="Emergency Ward"
                      | Cath$Origin=="Transfer"
                      | Cath$Origin=="Transfer>>Emergent")
#Mark those procedures that happened entirely outside primetime
Cath$NonPrimetime = FALSE
Cath$NonPrimetime[(Cath$Pre8AMStartFlag & Cath$Out.RoomT<Cath$Date8am)] = TRUE
Cath$NonPrimetime[(Cath$Post6PMEndFlag & Cath$In.RoomT>Cath$Date6pm)] = TRUE
table(Cath$NonPrimetime)
sum(Cath$RoomTimePlusTOMinutes[Cath$NonPrimetime & Cath$Date=='2014-05-01'])

Cath = subset(Cath,!NonPrimetime)
Cath$NonPrimetime = NULL

########Not relevant for EP#############
#Mark those procedures that started outside the 8am-6pm prime time hours and mark emergency procedures
#EP$Pre8AMStartFlag = (EP$Date8am > EP$CaseStart)
#EP$Post6PMEndFlag = (EP$Date6pm < EP$CaseEnd)




#Dates for 25 weeks
Dates = data.frame(Date=seq(as.Date(min(Cath$Date)),as.Date(max(Cath$Date)), by = 1))
Dates$Weekday = weekdays(Dates$Date)
Dates = subset(Dates, Dates$Weekday != "Saturday" & Dates$Weekday != "Sunday")
Dates = Dates[order(Dates$Date),]
Dates = Dates[4:128,]
Dates$DayOfPeriod = 1:125
Dates$WeekOfPeriod = rep(1:25, each = 5)
EP.Period = merge(x=Dates,y=EP,by="Date",all.x=TRUE)
Cath.Period = merge(x=Dates,y=Cath,by="Date",all.x=TRUE)


# add schedule horizon to both datasets
Cath.Period$ScheduleHorizon = 2
Cath.Period$ScheduleHorizon[Cath.Period$Origin %in% c('RPPR','Outpatient','SDA')] = 3
Cath.Period$ScheduleHorizon[Cath.Period$Origin %in% c('Emergency Room','Emergency Ward','Transfer>>Emergent')] = 1

EPScheduling = read.csv('EPProcs.csv')
EPScheduling = subset(EPScheduling,Schedule.horizon %in% c('1','2','3'))
EPScheduling = EPScheduling[,c(1,6)]
EPTest = merge(x=EP.Period,y=EPScheduling,'ProcedureTypeOfStudy',all.x=TRUE)
EPTest = subset(EPTest,!duplicated(SS_Event_EP_ID))
EPTest = subset(EPTest,ProcedureTypeOfStudy!='Angiography')
EPTest$Schedule.horizon[is.na(EPTest$Schedule.horizon)] = 2
EPTest$ScheduleHorizon = as.character(EPTest$Schedule.horizon)
EPTest$Schedule.horizon = NULL
EP.Period = EPTest



##################Add procedure names used for scheduling to Cath
CathProcNames = read.csv("CathERequest.csv")
names(CathProcNames)[5]="Patient_ID"
CathTest = merge(Cath.Period, CathProcNames, "Patient_ID", all.x=TRUE)
CathTest = subset(CathTest, !duplicated(SS_Event_Cath_ID))
names(CathTest)[names(CathTest) == "Procedure.1"] = "Procedure"
CathTest$ProcCat = as.character(CathTest$Proc_Group)
CathTest$Procedure = as.character(CathTest$Procedure)
CathTest$Procedure[is.na(CathTest$Procedure) | CathTest$Procedure == ""] = CathTest$ProcCat[is.na(CathTest$Procedure)| CathTest$Procedure == ""]
Cath.Period = CathTest
Cath.Period$ProcCat2 = Cath.Period$ProcCat
Cath.Period$ProcCat = Cath.Period$Procedure

##########Add room move felxibility

#Match procedure names between files
EP.Period$ProcCat = EP.Period$ProcedureTypeOfStudy
Cath.Period$ProcCat = Cath.Period$Proc_Group

#Only allow devices, pacemakers and ICDs but not leads, to be performed in either lab
Devices = as.character(EP.Period$ProcCat[ grepl("ICD", EP.Period$ProcCat) |
                                            grepl("pacemaker", EP.Period$ProcCat)])
EP.Period$PossibleRooms = 1
EP.Period$PossibleRooms[EP.Period$ProcCat %in% unique(Devices)] = 2
EP.Period$PossibleRooms[EP.Period$ProcedureRoom =="Middle Room" ] = 3

#Only allow Temp Wires and 
Cath.Period$PossibleRooms = 0
CathTemp = as.character(Cath.Period$ProcCat[ grepl("temp", Cath.Period$ProcCat) | 
                                              grepl("pacemaker", Cath.Period$ProcCat) |
                                              grepl("pericardial", Cath.Period$ProcCat)])
Cath.Period$PossibleRooms[Cath.Period$ProcCat %in% unique(CathTemp)] = 2


######Add pre and post procedures times
#Catagorize the patient post-proc times in Excel and format the data to merge with the data set
CathBays =read.csv("CathRecoveryBayCatagories.csv")
EPBays = read.csv("EPRecoveryBayCatagories.csv")

#Merge this data with the existing DBs
Cath.Period = merge(Cath.Period, CathBays, by = "SS_Event_Cath_ID", all.x=TRUE)
EP.Period = merge(EP.Period, EPBays, by = "ProcCat", all.x = TRUE)


#########The following was done once to create the CathBays and EPBays files

#CathBay = Cath.Period[,c("Origin", "Destination", "ProcCat2", "SS_Event_Cath_ID")] 
#CathBay$PreProcTime = 1
#CathBay$PostProcTime = 2
#PCIs = read.csv("JanSep14PCIs.csv")
#CathBayDetail = merge(x=CathBay, y=PCIs, by = "SS_Event_Cath_ID", all.x=TRUE)



###############Set pre-proc times


#For procedures with no pre-proc time, we sampled from the distribution of pre-proc times for the same procedure
#Cath.Period$PreProcTime = Cath.Period$PreProcTime

#For EP we generate random normal data with mean 1 hour and SD 5 minutes
EP.Period$PreProcTime = 0
EP.Period$PreProcTime = rnorm(length(EP.Period$PreProcTime),1,1/12)

#Set default post-proc times to be modified based on catagories
Cath.Period$PostProcTime = 2
EP.Period$PostProcTime = 2

#For the uncatagorized procedures, sample from the catagorized ones.
Cath.Period$PostProcCat = as.character(Cath.Period$PostProcCat)
for (i in 1:sum(is.na(Cath.Period$PostProcCat))){
  Cath.Period$PostProcCat[is.na(Cath.Period$PostProcCat)][i] = SampleFunction(i,"PostProcCat", "Proc_Group", subset(Cath.Period,is.na(PostProcCat)),subset(Cath.Period,!is.na(PostProcCat)))
}
Cath.Period$PostProcCat[is.na(Cath.Period$PostProcCat)] = "Cat1"

#Modify for Cath using the catagories and data from the Excel sheet titled CathPostProcCatagories.xlsx
#Introduce a column called Random Number to randomize between procedures 
#that require different amounts of Post-Proc time in each catagory
Cath.Period$RandomNumber = 100*runif(length(Cath.Period[,1]))

#Catagory 1 has 3 subcatagories. For these sub-catagories we use PtSubset to choose the appropriate percetnage of the 
#patients to include. This number is given in the Excel sheet.
#Once this subset is defined, the function sum(PtSubset) gives the length of the subset as the first parameter
#of the random number generator for how many numbers to generate
PtSubset = Cath.Period$PostProcCat == "Cat1" & Cath.Period$RandomNumber <= 55
Cath.Period$PostProcTime[PtSubset] =  rnorm(sum(PtSubset),3,1/6)

PtSubset = Cath.Period$PostProcCat == "Cat1" & Cath.Period$RandomNumber > 55 & Cath.Period$RandomNumber <= 85
Cath.Period$PostProcTime[PtSubset] =  rnorm(sum(PtSubset),4.5,1/5)

PtSubset = Cath.Period$PostProcCat == "Cat1" & Cath.Period$RandomNumber > 85
Cath.Period$PostProcTime[PtSubset] =  rnorm(sum(PtSubset),7,1/4)


Cath.Period$PostProcTime[Cath.Period$PostProcCat == "Cat2"] = rnorm(sum(Cath.Period$PostProcCat == "Cat2"), 8, 1/4)
Cath.Period$PostProcTime[Cath.Period$PostProcCat == "Cat3"] = rnorm(sum(Cath.Period$PostProcCat == "Cat3"), 8, 1/4) 
Cath.Period$PostProcTime[Cath.Period$PostProcCat == "Cat4"] = rnorm(sum(Cath.Period$PostProcCat == "Cat4"), .25, .05)
Cath.Period$PostProcTime[Cath.Period$PostProcCat == "Cat5"] = rnorm(sum(Cath.Period$PostProcCat == "Cat5"), 8, 1/4)

#Catagory 6 has 2 subcatagories
PtSubset = Cath.Period$PostProcCat == "Cat6" & Cath.Period$RandomNumber <= 65
Cath.Period$PostProcTime[PtSubset] =  rnorm(sum(PtSubset),3,1/6)

PtSubset = Cath.Period$PostProcCat == "Cat6" & Cath.Period$RandomNumber > 65
Cath.Period$PostProcTime[PtSubset] =  rnorm(sum(PtSubset),1,1/8)

Cath.Period$PostProcTime[Cath.Period$PostProcCat == "Cat7"] = 8*runif(sum(Cath.Period$PostProcCat == "Cat7"))



#First add 0 post proc time to Middle Room procedures
EP.Period$PostProcCat = as.character(EP.Period$PostProcCat)
EP.Period$PostProcCat[is.na(EP.Period$PostProcCat)] = "MR"
EP.Period$PostProcTime[EP.Period$PostProcCat == "MR"] = 0
#Modify for EP using the catagories and data from the Excel sheet titled
EP.Period$PostProcTime[EP.Period$PostProcCat == "Cat1"] = rnorm(sum(EP.Period$PostProcCat == "Cat1"), 1.5, 1/8)
EP.Period$PostProcTime[EP.Period$PostProcCat == "Cat2"] = 4+2*runif(sum(EP.Period$PostProcCat == "Cat2"))
EP.Period$PostProcTime[EP.Period$PostProcCat == "Cat3"] = 1/2+1/2*runif(sum(EP.Period$PostProcCat == "Cat3"))
EP.Period$PostProcTime[EP.Period$PostProcCat == "Cat4"] = rnorm(sum(EP.Period$PostProcCat == "Cat4"), 3, 1/6)
EP.Period$PostProcTime[EP.Period$PostProcCat == "Cat5"] = 1/2+1.5*runif(sum(EP.Period$PostProcCat == "Cat5"))
EP.Period$PostProcTime[EP.Period$PostProcCat == "CatNA"] = 0



#Write the intermediate files before merging and removing columns. This will be used in the volume projections
write.csv(Cath.Period, "AllCathDataRestricted.csv")
write.csv(EP.Period, "AllEPDataRestricted.csv")




# extract relevant information and merge lab info
Cath.Period$SS_Event_ID = Cath.Period$SS_Event_Cath_ID
EP.Period$SS_Event_ID = EP.Period$SS_Event_EP_ID

Cath2 = Cath.Period[,c("DayOfPeriod","WeekOfPeriod", "Lab", "RoomTimePlusTOMinutes", "ScheduleHorizon",
                       "PossibleRooms", "PreProcTime", "PostProcTime","ProcCat", "Attending", "PostProcCat", "SS_Event_ID") ]
EP2 = EP.Period[,c("DayOfPeriod","WeekOfPeriod", "Lab", "RoomTimePlusTOMinutes", "ScheduleHorizon",
                   "PossibleRooms", "PreProcTime", "PostProcTime","ProcCat", "Attending", "PostProcCat", "SS_Event_ID") ]
CATHandEP = rbind(Cath2,EP2)

# create a procedure key
Procs = data.frame(ProcCat = unique(CATHandEP$ProcCat), ProcKey = 1:length(unique(CATHandEP$ProcCat)))
write.csv(Procs, "ProcedureKey.csv")

Attendings = data.frame(Attending = unique(CATHandEP$Attending), AttendingKey = 1:length(unique(CATHandEP$Attending)))
write.csv(Attendings, "AttendingKey.csv")

CATHandEP = merge(CATHandEP,Procs,"ProcCat")
CATHandEP = merge(CATHandEP,Attendings,"Attending")
CATHandEP = subset(CATHandEP,!is.na(RoomTimePlusTOMinutes))
write.csv(CATHandEP, "CathAndEPFull.csv")

#Will write files after volume changes are complete
#Remove the columns that won't be used for the Python script
#CATH.EP.ForPython = CATHandEP[,c("DayOfPeriod","WeekOfPeriod", "Lab", "RoomTimePlusTOMinutes", "ScheduleHorizon",
                                 "PossibleRooms", "PreProcTime", "PostProcTime","ProcKey", "AttendingKey")]



#write.table(CATH.EP.ForPython, file='CathAndEP_Oct15.csv', row.names=FALSE, col.names=FALSE,sep=",")


###################Incorporate Volume Changes#################

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

#Look at the overall case mix. Drop the procedures that were aborted
Volumes = subset(Volumes, ProcCat != "Case Aborted")
Volumes = subset(Volumes, ProcCat != "Aborted")

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
  ExtraProvider$ScheduleHorizon = 3
  
  ExtraProvider$Attending = "New EP Provider 1"
  ExtraProvider$AttendingKey = max(ExtraProvider$AttendingKey)+1
  
  ExtraProvider2 = ExtraProvider
  ExtraProvider2$Attending = "New EP Provider 2"
  ExtraProvider2$AttendingKey = max(ExtraProvider2$AttendingKey)+1
  
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


OutputDir='/Users/dscheink/Documents/MIT-MGH/EP_Cath/Git/mghSchedulingModel/InputData/'

write.table(ForPython(CathFlatEPFlat), file=paste(OutputDir,'CathFlatEPFlatV2.csv',sep=""), row.names=FALSE, col.names=FALSE,sep=",")
write.table(ForPython(CathFlatEPGrow1), file=paste(OutputDir,'CathFlatEPGrow1V2.csv', sep=""), row.names=FALSE, col.names=FALSE,sep=",")
write.table(ForPython(CathFlatEPGrow2), file=paste(OutputDir,'CathFlatEPGrow2V2.csv', sep=""), row.names=FALSE, col.names=FALSE,sep=",")

write.table(ForPython(CathDrop1EPFlat), file=paste(OutputDir,'CathDrop1EPFlatV2.csv', sep=""), row.names=FALSE, col.names=FALSE,sep=",")
write.table(ForPython(CathDrop1EPGrow1), file=paste(OutputDir,'CathDrop1EPGrow1V2.csv', sep=""), row.names=FALSE, col.names=FALSE,sep=",")
write.table(ForPython(CathDrop1EPGrow2), file=paste(OutputDir,'CathDrop1EPGrow2V2.csv', sep=""), row.names=FALSE, col.names=FALSE,sep=",")

write.table(ForPython(CathDrop2EPFlat), file=paste(OutputDir,'CathDrop2EPFlatV2.csv', sep=""), row.names=FALSE, col.names=FALSE,sep=",")
write.table(ForPython(CathDrop2EPGrow1), file=paste(OutputDir,'CathDrop2EPGrow1V2.csv', sep=""), row.names=FALSE, col.names=FALSE,sep=",")
write.table(ForPython(CathDrop2EPGrow2), file=paste(OutputDir,'CathDrop2EPGrow2V2.csv', sep=""), row.names=FALSE, col.names=FALSE,sep=",")

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
