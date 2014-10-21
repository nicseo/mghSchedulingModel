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

#setwd("/Users/dscheink/Documents/MIT-MGH/EP_Cath/R_PythonAnalysis/Joint Unit Model/ReModifiedAlgorithm")
setwd("/Users/nicseo/Desktop/MIT/Junior/Fall/UROP/Scheduling Optimization/Script/RScriptsAndData")

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
EP = subset(EP, ProcedureRoom %in% c("EPLAB1","EPLAB2","EPLAB3","Ellison9lab"))

# add information about the labs that the procedures took place in
Cath$Lab = 0
EP$Lab = 1

################ DATE FORMATTING ################

# format dates to be readable by R and only keep usable subset of data
Cath$Date0am = as.POSIXlt(paste(Cath$Date_of_Cath, "00:00:01 AM", sep = " "),format = "%m/%d/%y %H:%M:%S")
Cath$Date8am = as.POSIXlt(paste(Cath$Date_of_Cath, "8:00:00 AM", sep = " "),format = "%m/%d/%y %r")
Cath$Date6pm = as.POSIXlt(paste(Cath$Date_of_Cath, "6:00:00 PM", sep = " "),format = "%m/%d/%y %r")
Cath$Date = as.Date(as.POSIXlt(Cath$Date_of_Cath, format = "%m/%d/%y"))
Cath$In.RoomT = as.POSIXlt(paste(Cath$Date_of_Cath, Cath$Patient_in_Room, sep = " "),format = "%m/%d/%y %r")
Cath$Out.RoomT = as.POSIXlt(paste(Cath$Date_of_Cath, Cath$PatientOutOfRoom, sep = " "),format = "%m/%d/%y %r")
Cath = subset(Cath, !is.na(In.RoomT) & !is.na(Out.RoomT))
Cath = subset(Cath, Cath$Out.RoomT > Cath$In.RoomT)

EP$Date0am = as.POSIXlt(paste(EP$EventDate, "00:00:01 AM", sep = " "),format = "%m/%d/%y %H:%M:%S")
EP$Date8am = as.POSIXlt(paste(EP$EventDate, "8:00:00 AM", sep = " "),format = "%m/%d/%y %r")
EP$Date6pm = as.POSIXlt(paste(EP$EventDate, "6:00:00 PM", sep = " "),format = "%m/%d/%y %r")
EP$Date = as.Date(as.POSIXlt(EP$EventDate, format = "%m/%d/%y"))
EP$CaseStart = as.POSIXlt(paste(EP$EventDate, EP$ProcTimeCaseStarted, sep = " "),format = "%m/%d/%y %r")
EP$CaseEnd = as.POSIXlt(paste(EP$EventDate, EP$ProcTimeCaseEnd, sep = " "),format = "%m/%d/%y %r")
#### TO DO: padding from a normal distribution ####
#EP$In.RoomT = as.POSIXlt(paste(EP$EventDate, EP$Patient_in_Room, sep = " "),format = "%m/%d/%y %r")
#EP$Out.RoomT = as.POSIXlt(paste(EP$EventDate, EP$PatientOutOfRoom, sep = " "),format = "%m/%d/%y %r")
EP = subset(EP, !is.na(CaseStart) & !is.na(CaseEnd))
EP = subset(EP, EP$CaseEnd > EP$CaseStart)

################ CATH IN ROOM TIME CALCULATION ################

Cath$WeekDay = weekdays(Cath$Date)
# lose pieces of a factor = convert to character
Cath$Date_of_Cath = as.character(Cath$Date_of_Cath)
Cath = Cath[Cath$WeekDay != "Saturday" & Cath$WeekDay != "Sunday",]
Cath$Room = as.character(Cath$Room)

#Calculate the in room time
Cath$RoomTime = difftime(Cath$Out.RoomT, Cath$In.RoomT, unit= "min")
# use for manipulations
Cath$RoomTimeMinutes = as.numeric(Cath$RoomTime)
#Add 35 for turnover time
Cath$RoomTimePlusTOMinutes = Cath$RoomTimeMinutes + pmin(Cath$RoomTimeMinutes,rnorm(length(Cath$RoomTimeMinutes),35,5))



#Mark those procedures that started outside the 8am-6pm prime time hours and mark emergency procedures
Cath$Pre8AMStartFlag = (Cath$Date8am > Cath$In.RoomT)
Cath$Post6PMEndFlag = (Cath$Date6pm < Cath$Out.RoomT)
Cath$EmergencyFlag = (Cath$Origin=="Emergency Room"
                      | Cath$Origin=="Emergency Ward"
                      | Cath$Origin=="Transfer"
                      | Cath$Origin=="Transfer>>Emergent")
#Mark those procedures that happened entirely outside primetime
Cath$NonPrimetime = 0
Cath$NonPrimetime[(Cath$Pre8AMStartFlag & Cath$Out.RoomT<Cath$Date8am)] = 1
Cath$NonPrimetime[(Cath$Post6PMEndFlag & Cath$In.RoomT>Cath$Date6pm)] = 1
table(Cath$NonPrimetime)
sum(Cath$RoomTimePlusTOMinutes[Cath$NonPrimetime & Cath$Date=='2014-05-01'])

Cath = subset(Cath,!NonPrimetime)
Cath$NonPrimetime = NULL

################ EP IN ROOM TIME CALCULATION ################

EP$WeekDay = weekdays(EP$Date)
#EP$Date_of_EP = as.character(EP$Date_of_EP)

#Calculate the case time
EP$CaseTime = difftime(EP$CaseEnd,EP$CaseStart, unit = "min")
EP$CaseTimeMinutes = as.numeric(EP$CaseTime)
EP$RoomTimePlusTOMinutes = EP$CaseTimeMinutes + pmin(EP$CaseTimeMinutes,rnorm(length(EP$CaseTimeMinutes),85,10))

#Mark those procedures that started outside the 8am-6pm prime time hours and mark emergency procedures
EP$Pre8AMStartFlag = (EP$Date8am > EP$CaseStart)
EP$Post6PMEndFlag = (EP$Date6pm < EP$CaseEnd)



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

#Only allow Temp Wires and 
Cath.Period$PossibleRooms = 0
CathTemp = as.character(Cath.Period$ProcCat[ grepl("temp", Cath.Period$ProcCat) | 
                                              grepl("pacemaker", Cath.Period$ProcCat) |
                                              grepl("pericardial", Cath.Period$ProcCat)])
Cath.Period$PossibleRooms[Cath.Period$ProcCat %in% unique(CathTemp)] = 2


######Add pre and post procedures times

CathBay = Cath.Period[,c("Origin", "Destination", "ProcCat2", "SS_Event_Cath_ID")] 
CathBay$ProcProcTime = 1
CathBay$PostProcTime = 2
PCIs = read.csv("JanSep14PCIs.csv")

CathBayDetail = merge(x=CathBay, y=PCIs, by = "SS_Event_Cath_ID", all.x=TRUE)

#Catagorize the patient post-proc times in Excel and format the data to merge with the data set
CathBays =read.csv("CathRecoveryBayCatagories.csv")
EPBays = read.csv("EPRecoveryBayCatagories.csv")

#Merge this data with the existing DBs
Cath.Period = merge(Cath.Period, CathBays, by = "SS_Event_Cath_ID")
EP.Period = merge(EP.Period, EPBays, by = "ProcCat")

#Set pre-proc times
Cath.Period$PreProcTime = 1
EP.Period$PreProcTime = 1

#Set default post-proc times to be modified based on catagories
Cath.Period$PostProcTime = 2
EP.Period$PostProcTime = 2

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

#Modify for EP using the catagories and data from the Excel sheet titled
EP.Period$PostProcTime[EP.Period$PostProcCat == "Cat1"] = rnorm(sum(EP.Period$PostProcCat == "Cat1"), 1.5, 1/8)
EP.Period$PostProcTime[EP.Period$PostProcCat == "Cat2"] = 4+2*runif(sum(EP.Period$PostProcCat == "Cat2"))
EP.Period$PostProcTime[EP.Period$PostProcCat == "Cat3"] = 1/2+1/2*runif(sum(EP.Period$PostProcCat == "Cat3"))
EP.Period$PostProcTime[EP.Period$PostProcCat == "Cat4"] = rnorm(sum(EP.Period$PostProcCat == "Cat4"), 3, 1/6)
EP.Period$PostProcTime[EP.Period$PostProcCat == "Cat5"] = 1/2+1.5*runif(sum(EP.Period$PostProcCat == "Cat5"))
EP.Period$PostProcTime[EP.Period$PostProcCat == "CatNA"] = 0

#Include event ID
Cath.Period$SS_Event_ID = Cath.Period$SS_Event_Cath_ID
EP.Period$SS_Event_ID = EP.Period$SS_Event_EP_ID

#Write the intermediate files before merging and removing columns. This will be used in the volume projections
write.csv(Cath.Period, "AllCathDataRestricted.csv")
write.csv(EP.Period, "AllEPDataRestricted.csv")




# extract relevant information and merge lab info
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

#Remove the columns that won't be used for the Python script
CATH.EP.ForPython = CATHandEP[,c("DayOfPeriod","WeekOfPeriod", "Lab", "RoomTimePlusTOMinutes", "ScheduleHorizon",
                                 "PossibleRooms", "PreProcTime", "PostProcTime","ProcKey", "AttendingKey")]

write.table(CATH.EP.ForPython, file='CathAndEP_Oct15.csv', row.names=FALSE, col.names=FALSE,sep=",")




