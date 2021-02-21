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
ggsurvplot(fit_1, data = KaplanMeier, palette = c("red", "blue"), break.x.by = 5,
           conf.int = T, pval = T, 
           surv.median.line = "hv", 
           title = "", xlab = "Years Served", ylab = "Proportion of Police Officers",
           legend = c(0.8, 0.7), 
           legend.title = "Groups:",
           legend.labs = c("Female", "Male"),
           xlim = c(0,45),
           pval.method = T,
           censor = T,
           risk.table = T,
           censor.shape = 124,
           censor.size = 2)



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
#whiteVsHispanic = subset(KaplanMeier, KaplanMeier$race == 'WHITE' | KaplanMeier$race == "HISPANIC")
Hispanic = subset(KaplanMeier, KaplanMeier$race == "HISPANIC")

#Fit the Kaplan Meier curve by White and Hispanic

#surv_object = survival::Surv(time = whiteVsHispanic$years_served, event = whiteVsHispanic$event)
#fit_3 = survminer::surv_fit(surv_object ~ whiteVsHispanic$race, data = whiteVsHispanic)
#summary(fit_3)
surv_object = survival::Surv(time = Hispanic$years_served, event = Hispanic$event)
fit_3 = survminer::surv_fit(surv_object ~ Hispanic$race, data = Hispanic)
summary(fit_3)



#Plotting the Kaplan Meier Curve by White Hispanic
library(survminer)
ggsurvplot(fit_3, data = Hispanic, pval = T, 
           combine = T, xlim = c(0,45))



#Making subsets of data based on race
whiteVsBlackVsHispanic = subset(KaplanMeier, KaplanMeier$race == 'WHITE' | KaplanMeier$race == "BLACK" | KaplanMeier$race == "HISPANIC")
#Fit the Kaplan Meier curve by White, Hispanic and Black
surv_object = survival::Surv(time = whiteVsBlackVsHispanic$years_served, event = whiteVsBlackVsHispanic$event)
fit_4 = survminer::surv_fit(surv_object ~ whiteVsBlackVsHispanic$race, data = whiteVsBlackVsHispanic)
summary(fit_4)

#Plotting the Kaplan Meier Curve by White, Hispanic and Black
library(survminer)
#ggsurvplot(fit_4, data = whiteVsBlackVsHispanic, pval = T, xlim = c(0,45))
ggsurvplot(fit_4, data = whiteVsBlackVsHispanic, 
           palette = c("orange", "green", "blue"), break.x.by = 5,
           conf.int = T, pval = T, 
           surv.median.line = "hv", 
           title = "", xlab = "Years Served", ylab = "Proportion of Police Officers",
           legend = c(0.8, 0.7), 
           legend.title = "Groups:",
           legend.labs = c("Black", "Hispanic", "White"),
           xlim = c(0,45),
           pval.method = T,
           censor = T,
           risk.table = T,
           censor.shape = 124,
           censor.size = 2)