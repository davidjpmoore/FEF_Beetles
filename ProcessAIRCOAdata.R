#purpose: Calculate diurnal change in CO2 concentration at FEF after rise in atmospheric background has been removed by substracting observations from SPL http://www.eol.ucar.edu/homes/stephens/RACCOON/index.html
#author: Dave Moore
#Date: 03/31/2015
#Load data from rdata file
load (file="Aircoa.rdata")
# 
FEFACO$rdate = ISOdatetime(FEFACO$YEAR, FEFACO$MON, FEFACO$DAY, FEFACO$HOUR, FEFACO$MIN, sec=0, tz="GMT")
# 
SPLACO$rdate = ISOdatetime(SPLACO$YEAR, SPLACO$MON, SPLACO$DAY, SPLACO$HOUR, SPLACO$MIN, sec=0, tz="GMT" )
# # 
# # America/Denver *
# 
FEFACO$LTdate = FEFACO$rdate
attributes(FEFACO$LTdate)$tzone <- "America/Denver"
FEFACO$LThour <- as.numeric(strftime(FEFACO$LTdate, format = "%H"))

FEFACO$LTday = as.numeric(strftime(FEFACO$LTdate, format = "%d"))
FEFACO$LTyear = as.numeric(strftime(FEFACO$LTdate, format = "%Y"))

FEFACO$LTmonth = as.numeric(strftime(FEFACO$LTdate, format = "%m"))
FEFACO06 = filter(FEFACO, FEFACO$LTyear>2005)

FEFACO06$timestep = 1:length(FEFACO06$YEAR.FRAC)
stepsPerDay = 24*4
FEFACO06$DayIndex = floor(FEFACO06$timestep/stepsPerDay)

FEFACOmonthly = filter (FEFACO06, FEFACO06$LTmonth==7)
MinCO2_1= tapply(FEFACOmonthly$CO21, FEFACOmonthly$DayIndex, min, na.rm = TRUE)
MaxCO2_1= tapply(FEFACOmonthly$CO21, FEFACOmonthly$DayIndex, max, na.rm = TRUE)
YearDiurnal= tapply(FEFACOmonthly$LTyear, FEFACOmonthly$DayIndex, min, na.rm = TRUE)
mONTHDiurnal= tapply(FEFACOmonthly$LTmonth, FEFACOmonthly$DayIndex, min, na.rm = TRUE)

FactorYearDiurnal = as.factor(YearDiurnal)
DiurnalCO2_1 = MaxCO2_1-MinCO2_1


DiurnalCO2_1[DiurnalCO2_1>300]=NA
DiurnalCO2_1[DiurnalCO2_1==0]=NA
TSD = YearDiurnal-2006
boxplotsDiurnalJuly = data.frame(TSD,YearDiurnal,mONTHDiurnal,FactorYearDiurnal,DiurnalCO2_1)

p <- ggplot(boxplotsDiurnalJuly, aes(factor(TSD), DiurnalCO2_1))
p + geom_boxplot() +geom_jitter(alpha=0.25, height = 0, size =2) +theme_bw() +labs(x = "Years since outbreak", y = "Summertime Diurnal CO2 Change", size=10) + guides(color = "none")

ggsave("FEFdiurnal_TSD.png", width = 8, height = 5)

p + geom_boxplot()

write.csv(boxplotsDiurnalJuly,'boxplotsDiurnalJuly.csv')

medAnnual_DiurnalCO2_1 = tapply(DiurnalCO2_1, YearDiurnal, quantile, probs=0.50, na.rm = TRUE)
Annual_DiurnalCO2_1_25p = tapply(DiurnalCO2_1, YearDiurnal, quantile, probs=0.25, na.rm = TRUE)
Annual_DiurnalCO2_1_75p = tapply(DiurnalCO2_1, YearDiurnal, quantile, probs=0.75, na.rm = TRUE)
BoxDiurnalCO2 =Annual_DiurnalCO2_1_75p - Annual_DiurnalCO2_1_25p
TSD = as.factor(0:8)
DiurnalCO2 =data.frame(TSD, medAnnual_DiurnalCO2_1, Annual_DiurnalCO2_1_25p, Annual_DiurnalCO2_1_75p, BoxDiurnalCO2)
library(ggplot2)
h <- ggplot(DiurnalCO2, aes(x=TSD))
h + geom_ribbon(aes(ymin=Annual_DiurnalCO2_1_25p, ymax=Annual_DiurnalCO2_1_75p, color=)) + geom_line(aes(y=medAnnual_DiurnalCO2_1))



plot(medAnnual_DiurnalCO2_1)
plot (YearDiurnal,DiurnalCO2_1)

plot (FEFACO06$DayIndex, FEFACO06$CO21)

tbl_df(junk1)


# 
# SPLACO$LTdate = SPLACO$rdate
# attributes(SPLACO$LTdate)$tzone <- "America/Denver"
# SPLACO$LThour <- as.numeric(strftime(SPLACO$LTdate, format = "%H"))
# 

plot (junk1,FEFACO$CO21)
plot (FEFACO$LThour,FEFACO$CO21)

#can't figure out how to read lines 18 and 19 as headers using this approach
for (i in 1:length(SPLACO$LThour)) {
  
  assign(substr(tempFilelist[i], 5, 14), read.csv(file = paste0("data/FluxData/",tempFilelist[i]),skip=20,header=FALSE, na.strings=c('-9999','-6999'), stringsAsFactors=FALSE))  
}



FEFACO$CO21

HourlyCO2_1= tapply(FEFACO$CO21, FEFACO$LThour, mean, na.rm = TRUE)
plot(HourlyCO2_1)

library(dplyr)

tbl_df(FEFACO)
junk1 = filter(FEFACO, YEAR>2013)
plot (FEFACO$YEAR.FRAC, FEFACO$CO21)

