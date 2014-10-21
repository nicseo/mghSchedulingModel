library(data.table)
set.seed(123)

setwd("/Users/nicseo/Desktop/MIT/Junior/Fall/UROP/Scheduling Optimization/Script/RScriptsAndData")
Cath = read.csv("JanJun14CathTimesProvider.csv")
EPAll = read.csv("JanJun14EPTimesProvider.csv")
MiddleRoom = subset(EPAll, ProcedureRoom=="Middle Room")

MiddleRoom$Date0am = as.POSIXlt(paste(MiddleRoom$EventDate, "00:00:01 AM", sMiddleRoom = " "),format = "%m/%d/%y %H:%M:%S")
MiddleRoom$Date8am = as.POSIXlt(paste(MiddleRoom$EventDate, "8:00:00 AM", sMiddleRoom = " "),format = "%m/%d/%y %r")
MiddleRoom$Date6pm = as.POSIXlt(paste(MiddleRoom$EventDate, "6:00:00 PM", sMiddleRoom = " "),format = "%m/%d/%y %r")
MiddleRoom$Date = as.Date(as.POSIXlt(MiddleRoom$EventDate, format = "%m/%d/%y"))
MiddleRoom$CaseStart = as.POSIXlt(paste(MiddleRoom$EventDate, MiddleRoom$ProcTimeCaseStarted, sMiddleRoom = " "),format = "%m/%d/%y %r")
MiddleRoom$CaseEnd = as.POSIXlt(paste(MiddleRoom$EventDate, MiddleRoom$ProcTimeCaseEnd, sMiddleRoom = " "),format = "%m/%d/%y %r")


############## Day of period / Week of period ############## 
Dates = data.frame(Date=seq(as.Date(min(MiddleRoom$Date)),as.Date(max(MiddleRoom$Date)), by = 1))
Dates$Weekday = weekdays(Dates$Date)
Dates = subset(Dates, Dates$Weekday != "Saturday" & Dates$Weekday != "Sunday")
Dates = Dates[order(Dates$Date),]
Dates = Dates[4:128,]
Dates$DayOfPeriod = 1:125
Dates$WeekOfPeriod = rep(1:25, each = 5)
MR.Period = merge(x=Dates,y=MiddleRoom,by="Date",all.x=TRUE)

############## Lab: all EP ############## 
MR.Period$Lab = 1

############## Schedule horizon ############## 
MR.Period$ScheduleHorizon = 2
MR.Period$ScheduleHorizon[MR.Period$ProcedureTypeOfStudy=='cardioversion'] = 1
MR.Period$ScheduleHorizon[MR.Period$ProcedureTypeOfStudy=='tilt table test'] = 3

############## Possible Rooms: all Middle Room (3) ############## 
MR.Period$PossibleRooms = 3


############## Procedure key and Attending key ############## 
Procs = data.frame(ProcedureTypeOfStudy = unique(MR.Period$ProcedureTypeOfStudy), ProcKey = 1:length(unique(MR.Period$ProcedureTypeOfStudy)))
write.csv(Procs, "MiddleRoom_ProcedureKey.csv")

Attendings = data.frame(Attending = unique(MR.Period$Attending), AttendingKey = 1:length(unique(MR.Period$Attending)))
write.csv(Attendings, "MiddleRoom_AttendingKey.csv")

MR.Period = merge(MR.Period,Procs,"ProcedureTypeOfStudy")
MR.Period = merge(MR.Period,Attendings,"Attending")

############## Pre and Post Procedures ############## 


############## In room time calculations ############## 

# calculate case time for viable procedures
viableProcs = subset(MR.Period, !is.na(CaseStart) & !is.na(CaseEnd))
viableProcs = subset(viableProcs, viableProcs$CaseEnd > viableProcs$CaseStart)
viableProcs$CaseTime = difftime(viableProcs$CaseEnd,viableProcs$CaseStart, unit = "min")
viableProcs$CaseTimeMinutes = as.numeric(viableProcs$CaseTime)
viableProcs$RoomTimePlusTOMinutes = viableProcs$CaseTimeMinutes + pmin(viableProcs$CaseTimeMinutes,rnorm(length(viableProcs$CaseTimeMinutes),85,10))

viableCardioversions = subset(viableProcs, ProcedureTypeOfStudy=='cardioversion')
viableCardioversions = viableCardioversions[viableCardioversions$CaseTimeMinutes<120,]
viableTiltTableTests = subset(viableProcs, ProcedureTypeOfStudy=='tilt table test')

# use these distributions to fill in missing case time information
subsetCardioversion = (MR.Period$ProcedureTypeOfStudy=='cardioversion' & (is.na(MR.Period$CaseStart) | is.na(MR.Period$CaseEnd)))
subsetCardioversion[is.na(subsetCardioversion)] = FALSE
subsetTiltTable = MR.Period$ProcedureTypeOfStudy=='tilt table test' & (is.na(MR.Period$CaseStart) | is.na(MR.Period$CaseEnd))
subsetTiltTable[is.na(subsetTiltTable)] = FALSE
MR.Period$CaseTimeMinutes[subsetCardioversion] = rlnorm(sum(subsetCardioversion),3.227434,0.7224102)
MR.Period$CaseTimeMinutes[subsetTiltTable] = rnorm(sum(subsetTiltTable),120,5)
MR.Period$RoomTimePlusTOMinutes = MR.Period$CaseTimeMinutes + pmin(MR.Period$CaseTimeMinutes,rnorm(length(MR.Period$CaseTimeMinutes),85,10))

# calculate case time for the viable procedures by hand
MR.Period$CaseTime[(!is.na(MR.Period$CaseStart) & (!is.na(MR.Period$CaseEnd))] = difftime(MR.Period$CaseEnd,MR.Period$CaseStart, unit = "min")
MR.Period$CaseTimeMinutes = as.numeric(viableProcs$CaseTime)
MR.Period$RoomTimePlusTOMinutes = viableProcs$CaseTimeMinutes + pmin(viableProcs$CaseTimeMinutes,rnorm(length(viableProcs$CaseTimeMinutes),85,10))



# #Calculate the in room time
# MiddleRoom$ProcTime = difftime(MiddleRoom$CaseEnd, MiddleRoom$CaseStart, unit= "min")
# # use for manipulations
# MiddleRoom$ProcTimeMinutes = as.numeric(MiddleRoom$ProcTime)
# 
# table(as.character(MiddleRoom$ProcedureTypeOfStudy))
# 
# summary(MiddleRoom$ProcTimeMinutes[!is.na(MiddleRoom$ProcTimeMinutes) & 
#                                      MiddleRoom$ProcTimeMinutes>0 &
#                                      MiddleRoom$ProcTimeMinutes<100 &
#                                      MiddleRoom$ProcedureTypeOfStudy == "cardioversion"])

