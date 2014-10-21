9*60/mean(CathFlatEPGrow2$RoomTimePlusTOMinutes)

mean(ProcsByDay$Freq[ProcsByDay$Freq > 1])/10

9*60/mean(CathFlatEPGrow2$RoomTimePlusTOMinutes) + mean(ProcsByDay$Freq[ProcsByDay$Freq > 1])/8

60/mean(CathFlatEPGrow2$RoomTimePlusTOMinutes[CathFlatEPFlat$Lab == 0])

60/mean(CathFlatEPGrow2$RoomTimePlusTOMinutes[CathFlatEPFlat$Lab == 1])

ProcsByDay=as.data.frame(table(CathFlatEPGrow2$DayOfPeriod))

mean(ProcsByDay$Freq[ProcsByDay$Freq > 1])
