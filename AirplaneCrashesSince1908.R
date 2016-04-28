# Airplane Crashes Since 1908
#------------------------------------------------------------------------
#Loading libraries
library(dplyr)
library(lubridate)
library(ggplot2)
library(lattice)
library(scatterplot3d)
#------------------------------------------------------------------------
#Loading data
AirplaneCrashesSince1908 <- read.csv("3-Airplane_Crashes_Since_1908.txt", stringsAsFactors =  FALSE)
summary(AirplaneCrashesSince1908)
str(AirplaneCrashesSince1908)
#------------------------------------------------------------------------
#Data perperation
#Preparing summary field to replace it with 7 factors only
#Assign search words to vectors
WeatherStr <- c("[Ll]ightning", "[Ff]og", "[Ww]eather", "[Ww]ind", "[Rr]ain", "[Ss]now", "[Tt]hunder", "[Vv]isibility", "[Cc]loud", "[Tt]urbulence", "[Ss]torm", "[Ii]cing")
TerrorStr <- c("[Tt]error", "[hH]ijack", "[bB]omb", "[Dd]etonat", "[Ee]xplos")
ControlStr <- c("[Cc]ontrol", "[Nn]avigation", "[Pp]ilot", "[Ee]ngine", "[Mm]echanic", "[Ff]ailure", "Error", " error", "[Ff]uel", "[Cc]rack", "[Ss]peed", "[Bb]roke", "[Ss]tress", "[Tt]est", "[Ww]ing", "[Mm]aintain", "[Ww]eight", "[cC]ompli", "[Pp]itch", "[Cc]rew", "[Vv]ibration", "[Ee]xercise", "[Ss]tall", "[Ss]pin", "[Pp]assenger", "[Ss]ystem", "[Cc]aptain")
FireStr <- c("[Ff]ire", "[Bb]urn", "[Ff]lame", "[Ee]xplosion", "[Ee]xplode")
AccidentStr <- c("[Aa]ccident", "[Cc]rash", "[Ss]truck", "[Cc]ollision", "[Dd]isappear", "[Ll]an", "[Rr]unway", "[Cc]ollid", "[Dd]itch", "[Hh]it", "[Ff]lew", "[Ii]mpact", "[Mm]ountain", "[Ss]ea", "[Cc]ash")
ShootStr <- c("[Ss]hoot", "[Ss]hell", "[Ss]hot")

#Search for patterns in summary and replace summary field with new values
for (i in WeatherStr) {
    AirplaneCrashesSince1908$Summary[grepl(i, AirplaneCrashesSince1908$Summary)] <- "Weather"    
}
for (i in ShootStr) {
    AirplaneCrashesSince1908$Summary[grepl(i, AirplaneCrashesSince1908$Summary)] <- "Shot Down"
}
for (i in TerrorStr) {
    AirplaneCrashesSince1908$Summary[grepl(i, AirplaneCrashesSince1908$Summary)] <- "Terrorist Attack"
}
for (i in ControlStr) {
    AirplaneCrashesSince1908$Summary[grepl(i, AirplaneCrashesSince1908$Summary)] <- "Loss of control"
}
for (i in FireStr) {
    AirplaneCrashesSince1908$Summary[grepl(i, AirplaneCrashesSince1908$Summary)] <- "Fire"
}
AirplaneCrashesSince1908$Summary[is.na(AirplaneCrashesSince1908$Summary)] <- "UNKNOWN"
AirplaneCrashesSince1908$Summary[grepl("[Uu][Nn][Kk][Nn][Oo][Ww][Nn]", AirplaneCrashesSince1908$Summary)] <- "UNKNOWN"
AirplaneCrashesSince1908$Summary[grepl("[Uu]ndetermin", AirplaneCrashesSince1908$Summary)] <- "UNKNOWN"
AirplaneCrashesSince1908$Summary[!nzchar(AirplaneCrashesSince1908$Summary)] <- "UNKNOWN"
for (i in AccidentStr) {
    AirplaneCrashesSince1908$Summary[grepl(i, AirplaneCrashesSince1908$Summary)] <- "Accident"
}
AirplaneCrashesSince1908$Summary[!(AirplaneCrashesSince1908$Summary %in% c("Weather", "Fire", "Shot Down", "Terrorist Attack", "Loss of control", "UNKNOWN", "Accident"))] <- "UNKNOWN"

#Make sure of factors levels in summary column
as.factor(AirplaneCrashesSince1908$Summary) %>% levels

#Assign Clean data to new data frame
planeCrashes <- tbl_df(AirplaneCrashesSince1908)
planeCrashes

#------------------------------------------------------------------------

#Just for Knowladge

#Number of Crashed planes with Unknown number of people onboard
sum(is.na(planeCrashes$Aboard))

#Number of Crashed planes with Unknown number of people died
sum(is.na(planeCrashes$Fatalities))

#Number of Crashed planes with Unknown number of people survived
sum(is.na(planeCrashes$Aboard - planeCrashes$Fatalities))

#------------------------------------------------------------------------

#Tasks

#Get yearly planes creashed report without the unkonwn data (Unknown data excluded)
YearlyPlanesCreashedReportKnown <- planeCrashes %>% mutate(Date = mdy(Date), Time = hm(Time), Year = year(Date)) %>% group_by(Year) %>% summarize(yearlyplanescrashed = n(), OnBoard = sum(Aboard, na.rm = T), survived = sum(Aboard, na.rm = T) - sum(Fatalities, na.rm = T), died = sum(Fatalities, na.rm = T))
View(YearlyPlanesCreashedReportKnown)
write.csv(YearlyPlanesCreashedReportKnown, "YearlyPlanesCreashedReportKnown.csv")
qplot(Year, yearlyplanescrashed, data = YearlyPlanesCreashedReportKnown,geom = c("point", "smooth")) + labs(title = "Yearly Planes Creashed Report (Known Data)")
qplot(Year, OnBoard, data = YearlyPlanesCreashedReportKnown,geom = c("point", "smooth")) + labs(title = "People Onboard Creashed Planes over years (Known Data)")
qplot(Year, survived, data = YearlyPlanesCreashedReportKnown,geom = c("point", "smooth")) + labs(title = "People Survived in the Creashed Planes over years (Known Data)")
qplot(Year, died, data = YearlyPlanesCreashedReportKnown,geom = c("point", "smooth")) + labs(title = "People Died on Creashed Planes over years (Known Data)")

#Get yearly planes creashed report including the unkonwn data
YearlyPlanesCreashedReportWithUnKnown <- planeCrashes %>% mutate(Date = mdy(Date), Time = hm(Time), Year = year(Date)) %>% group_by(Year) %>% summarize(yearlyplanescrashed = n(), OnBoard = sum(Aboard), survived = sum(Aboard) - sum(Fatalities), died = sum(Fatalities))
View(YearlyPlanesCreashedReportWithUnKnown)
write.csv(YearlyPlanesCreashedReportWithUnKnown, "YearlyPlanesCreashedReportWithUnKnown.csv")
qplot(Year, yearlyplanescrashed, data = YearlyPlanesCreashedReportWithUnKnown,geom = c("point", "smooth")) + labs(title = "Yearly Planes Creashed Report (WithUnknown Data)")
qplot(Year, OnBoard, data = YearlyPlanesCreashedReportWithUnKnown,geom = c("point", "smooth")) + labs(title = "People Onboard Creashed Planes over years (WithUnknown Data)")
qplot(Year, survived, data = YearlyPlanesCreashedReportWithUnKnown,geom = c("point", "smooth")) + labs(title = "People Survived in the Creashed Planes over years (WithUnknown Data)")
qplot(Year, died, data = YearlyPlanesCreashedReportWithUnKnown,geom = c("point", "smooth")) + labs(title = "People Died on Creashed Planes over years (WithUnknown Data)")

#Get crash report by operator and type of aircrafts
CrashByOperator <- planeCrashes %>% group_by(Operator, Type) %>% summarize(NumberOfCrashes = n())
write.csv(CrashByOperator, "CrashByOperatorandAircraftType.csv")

#Summary field contains categories
View(planeCrashes)
categoriesTable <- table(planeCrashes$Summary)
write.csv(planeCrashes, "PlaneCrashesAfterModifySummaryToCategories.csv")
write.csv(categoriesTable, "CategoriesTableWithAccedentsCount.csv")
qplot(Summary, data = planeCrashes) + labs(title = "Count Number Of Crashes for each Category") + labs(x = "Category", y = "Number of Crashes")

#Number of crashed aircrafts and number of recorded deaths against each category from the summary field
planeCrashesWithCatKnown <- planeCrashes %>% group_by(Summary) %>% summarize(NumberOfCrashes = sum(n()), NumberOfRecordedDeaths = sum(Fatalities, na.rm = T)) %>% arrange(desc(NumberOfRecordedDeaths))
planeCrashesWithCatKnown
write.csv(planeCrashesWithCatKnown, "planeCrashesWithCategoriesandRcordedDeath.csv")
qplot(Summary, NumberOfKnownDeaths, data = planeCrashesWithCatKnown) + labs(title = "Count Number Of Recorded Death for each Category") + labs(x = "Category", y = "Number of Recorded Death") + geom_point(size = 8)
