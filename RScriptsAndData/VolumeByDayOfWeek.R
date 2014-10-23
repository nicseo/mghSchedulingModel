setwd("/Users/dscheink/Documents/MIT-MGH/EP_Cath/Git/mghSchedulingModel/RScriptsAndData")

Cath.Period = read.csv("AllCathDataRestricted.csv")

CathElective = subset(Cath.Period, ScheduleHorizon == 3)

CathPhysicianElectiveProcsByDay = as.data.frame.matrix(table(CathElective$Attending, CathElective$Weekday))

CE =as.data.table(CathElective)

CathPhysicianElectiveProcTimeByDay = as.data.frame(CE[,sum(RoomTimeMinutes) ,by = c("Attending", "Weekday")])

CathElectiveProcTimeByDay = as.data.frame(CE[,sum(RoomTimeMinutes) ,by = c( "Weekday")])

CathElectiveProcTimeByDay$V1/max(CathElectiveProcTimeByDay$V1)*100

colSums(CathPhysicianElectiveProcsByDay)/max(colSums(CathPhysicianElectiveProcsByDay))*100


CathPhysicianElectiveProcsByDay