EPAll = read.csv("JanJun14EPTimesProvider.csv")
MiddleRoom = subset(EPAll, !(ProcedureRoom %in% c("EPLAB1","EPLAB2","EPLAB3","Ellison9lab")))

MiddleRoom$Date0am = as.POSIXlt(paste(MiddleRoom$EventDate, "00:00:01 AM", sMiddleRoom = " "),format = "%m/%d/%y %H:%M:%S")
MiddleRoom$Date8am = as.POSIXlt(paste(MiddleRoom$EventDate, "8:00:00 AM", sMiddleRoom = " "),format = "%m/%d/%y %r")
MiddleRoom$Date6pm = as.POSIXlt(paste(MiddleRoom$EventDate, "6:00:00 PM", sMiddleRoom = " "),format = "%m/%d/%y %r")
MiddleRoom$Date = as.Date(as.POSIXlt(MiddleRoom$EventDate, format = "%m/%d/%y"))
MiddleRoom$CaseStart = as.POSIXlt(paste(MiddleRoom$EventDate, MiddleRoom$ProcTimeCaseStarted, sMiddleRoom = " "),format = "%m/%d/%y %r")
MiddleRoom$CaseEnd = as.POSIXlt(paste(MiddleRoom$EventDate, MiddleRoom$ProcTimeCaseEnd, sMiddleRoom = " "),format = "%m/%d/%y %r")

#Calculate the in room time
MiddleRoom$ProcTime = difftime(MiddleRoom$CaseEnd, MiddleRoom$CaseStart, unit= "min")
# use for manipulations
MiddleRoom$ProcTimeMinutes = as.numeric(MiddleRoom$ProcTime)

summary(MiddleRoom$ProcTimeMinutes[!is.na(MiddleRoom$ProcTimeMinutes) & 
                                     MiddleRoom$ProcTimeMinutes>0 &
                                     MiddleRoom$ProcTimeMinutes<100 &
                                     MiddleRoom$ProcedureTypeOfStudy == "cardioversion"])

