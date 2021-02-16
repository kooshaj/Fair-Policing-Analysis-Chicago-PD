#Installing packages
install.packages("survival")
install.packages("survminer")

#Loading packages
library(survival)
library(survminer)
library(dplyr)

#Read in data
KaplanMeier <- read.csv("C:/Users/david/Desktop/DS3 - Fair Policing Project/R Code/KaplanMeier.csv", header = T)

#Creating survival object
surv_object = survival::Surv(time = KaplanMeier$years_served, event = KaplanMeier$event)
surv_object

#Fit the Kaplan Meier Curve by Gender
fit_1 = survminer::surv_fit(surv_object ~ KaplanMeier$gender, data = KaplanMeier)
summary(fit_1)

#Plotting the Kaplan Meier Curve by Gender
library(survminer)
ggsurvplot(fit_1, data = KaplanMeier, pval = T, xlim = c(0,45), censor.shape = 124)



#Making subsets of data based on race
whiteVsBlack = subset(KaplanMeier, KaplanMeier$race == 'WHITE' | KaplanMeier$race == "BLACK")
#Fit the Kaplan Meier curve by White and Black
surv_object = survival::Surv(time = whiteVsBlack$years_served, event = whiteVsBlack$event)
fit_2 = survminer::surv_fit(surv_object ~ whiteVsBlack$race, data = whiteVsBlack)
summary(fit_2)

#Plotting the Kaplan Meier Curve by White and Black
library(survminer)
ggsurvplot(fit_2, data = whiteVsBlack, pval = T, xlim = c(0,45))



#Making subsets of data based on race
whiteVsHispanic = subset(KaplanMeier, KaplanMeier$race == 'WHITE' | KaplanMeier$race == "HISPANIC")
#Fit the Kaplan Meier curve by White and Hispanic
surv_object = survival::Surv(time = whiteVsHispanic$years_served, event = whiteVsHispanic$event)
fit_2 = survminer::surv_fit(surv_object ~ whiteVsHispanic$race, data = whiteVsHispanic)
summary(fit_2)

#Plotting the Kaplan Meier Curve by White and Hispanic
library(survminer)
ggsurvplot(fit_2, data = whiteVsHispanic, pval = T, xlim = c(0,45))