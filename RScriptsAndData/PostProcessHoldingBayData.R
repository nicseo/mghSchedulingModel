HoldingBay = read.csv("/Users/dscheink/Documents/MIT-MGH/EP_Cath/Git/mghSchedulingModel/holdingBay.csv")

Resolution = 15

NumberOfTimeSlots = 24*60/Resolution

HB= data.frame(
min = rep(0,NumberOfTimeSlots),
Quantile5 = rep(0,NumberOfTimeSlots),
Quantile25 = rep(0,NumberOfTimeSlots),
Quantile50 = rep(0,NumberOfTimeSlots),
Quantile75 = rep(0,NumberOfTimeSlots),
Quantile95 = rep(0,NumberOfTimeSlots),
max = rep(0,NumberOfTimeSlots),
mean = rep(0,NumberOfTimeSlots)
)



for (i in 1:NumberOfTimeSlots){
  HB$min[i] = min(HoldingBay[,i+1])
  HB$Quantile5[i] = quantile(HoldingBay[,i+1],.05)
  HB$Quantile25[i] = quantile(HoldingBay[,i+1],.25)
  HB$Quantile50[i] = quantile(HoldingBay[,i+1],.5)
  HB$Quantile75[i] = quantile(HoldingBay[,i+1],.75)
  HB$Quantile95[i] = quantile(HoldingBay[,i+1],.95)
  HB$max[i] = max(HoldingBay[,i+1])
  HB$mean[i] = mean(HoldingBay[,i+1])
}

plot(1:NumberOfTimeSlots, HB$mean, main = "Mean Cath and EP recovery bay use
      (Pre - 1 hour, Post - Varies) ")
plot(1:NumberOfTimeSlots, HB$Quantile95, main = "Q95 Cath and EP recovery bay use
      (Pre - 1 hour, Post - Varies)")
plot(1:NumberOfTimeSlots, HB$max, main = "Max Cath and EP recovery bay use
      (Pre - 1 hour, Post - Varies)")

write.csv(t(HB), "HoldingBayOccupancy.csv")


#Number of days with occupancy over 19 bays:
DailyMax = data.frame(Day = HoldingBay[,1], DailyMax = apply(HoldingBay[,2:121], 1, max))
sum(DailyMax$DailyMax>19)
mean(DailyMax$DailyMax)

sum(HoldingBay$X19.0)
sum(HoldingBay$X20.0)
sum(HoldingBay$X21.0)
sum(HoldingBay$X22.0)
sum(HoldingBay$X23.0)
sum(HoldingBay$X24.0)

boxplot(, data = t(HoldingBay))

boxplot(as.matrix(HoldingBay[,16:44]),use.cols=TRUE)
