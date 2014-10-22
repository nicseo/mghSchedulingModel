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
Cath$In.LabT = as.POSIXlt(paste(Cath$Date_of_Cath, Cath$Arrived.to.HR, sep = " "),format = "%m/%d/%y %r")
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

################ CATH PRE ROOM and IN ROOM TIME CALCULATION ################

Cath$WeekDay = weekdays(Cath$Date)
# lose pieces of a factor = convert to character
Cath$Date_of_Cath = as.character(Cath$Date_of_Cath)
Cath = Cath[Cath$WeekDay != "Saturday" & Cath$WeekDay != "Sunday",]
Cath$Room = as.character(Cath$Room)

#Calculate the pre room time
Cath$PreProc = difftime(Cath$In.RoomT, Cath$In.LabT, unit= "min")
# use for manipulations
Cath$PreProcMinutes = as.numeric(Cath$PreProc)

hist(Cath$PreProcMinutes[!is.na(Cath$PreProcMinutes)& Cath$PreProcMinutes>0],20)
quantile(Cath$PreProcMinutes[!is.na(Cath$PreProcMinutes)& Cath$PreProcMinutes>0])
mean(Cath$PreProcMinutes[!is.na(Cath$PreProcMinutes)& Cath$PreProcMinutes>0])

#Calculate the in room time
Cath$RoomTime = difftime(Cath$Out.RoomT, Cath$In.RoomT, unit= "min")
# use for manipulations
Cath$RoomTimeMinutes = as.numeric(Cath$RoomTime)
#Add 35 for turnover time
Cath$RoomTimePlusTOMinutes = Cath$RoomTimeMinutes + pmin(Cath$RoomTimeMinutes,rnorm(length(Cath$RoomTimeMinutes),35,5))
