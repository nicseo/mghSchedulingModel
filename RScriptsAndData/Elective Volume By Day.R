CathDT = as.data.table(Cath.Period)

CathProcTimes = as.data.frame(CathDT[, list(mean(RoomTimePlusTOMinutes), quantile(RoomTimePlusTOMinutes)), by = ProcCat])

CathProcsInDay = as.data.frame(CathDT[,length(RoomTimePlusTOMinutes), by = DayOfPeriod])

CathProcsInDay[CathProcsInDay$V1 == max(CathProcsInDay$V1),]

mean(CathProcsInDay$V1)
quantile(CathProcsInDay$V1)

EPDT = as.data.table(EP.Period)

EPProcTimes = as.data.frame(EPDT[, list(mean(RoomTimePlusTOMinutes), quantile(RoomTimePlusTOMinutes)), by = ProcCat])

EPProcsInDay = as.data.frame(EPDT[,length(RoomTimePlusTOMinutes), by = DayOfPeriod])

EPProcsInDay[EPProcsInDay$V1 == max(EPProcsInDay$V1),]

mean(EPProcsInDay$V1)
quantile(EPProcsInDay$V1)

