# Airplane Crashes Since 1908
#------------------------------------------------------------------------
#Loading data
university <- read.csv("3-university.txt")
summary(university)
head(university)

#------------------------------------------------------------------------
#Data Overview using chart
require(rCharts)
DataOverview <- rCharts::rPlot(Freq ~ Gender | Admit, data = university, color = 'Admit', type = 'bar')
DataOverview$save("University_Gender_Admissions.html")
DataOverview

#------------------------------------------------------------------------
#First Solution
Gender_Admissions <- tapply(university$Freq, list(university$Gender, university$Admit), sum)
Gender_Admissions
rowSums(Gender_Admissions)
GenderAdmissionsProp <- prop.table(Gender_Admissions, margin = 1)
GenderAdmissionsProp

write.csv(Gender_Admissions, "University_Gender_Admissions.csv")
write.csv(GenderAdmissionsProp, "University_Gender_Admissions_probabilities.csv")

#------------------------------------------------------------------------
#Second Solution
xt <- xtabs(Freq ~ Gender + Admit, data = university)
xt

#------------------------------------------------------------------------
#Third Solution
aa <- subset(university, university$Gender == "Male")
bb <- subset(university, university$Gender == "Female")
sum(aa$Freq)
sum(bb$Freq)
sum(subset(aa, aa$Admit == "Admitted")$Freq) / sum(aa$Freq)
sum(subset(bb, bb$Admit == "Admitted")$Freq) / sum(bb$Freq)
sum(subset(university, university$Admit == "Admitted" & university$Gender == "Male")$Freq) / sum(subset(university, university$Gender == "Male")$Freq)
sum(subset(university, university$Admit == "Admitted" & university$Gender == "Female")$Freq) / sum(subset(university, university$Gender == "Female")$Freq)
