###############   PSQF 7375 Advanced Longitudinal Example 2 using R   #############

# Set width of output and number of significant digits printed,
# number of digits before using scientific notation, shut off significance stars
options(width=120, digits=8, scipen=9, show.signif.stars=FALSE)

#####  Check to see if packages are downloaded, install if not, then load  #####

if (!require("haven")) install.packages("haven")
library(haven) # To import SAS data with labels as table

if (!require("expss")) install.packages("expss")
library(expss) # To add variable and value labels, sort

if (!require("TeachingDemos")) install.packages("TeachingDemos")
library(TeachingDemos) # To create text output files

if (!require("psych")) install.packages("psych")
library(psych) # To add summary functions

#if (!require("nlme")) install.packages("nlme")
#library(nlme) # To estimate MLMs with link functions and R cov structures

#if (!require("multcomp")) install.packages("multcomp")
#library(multcomp) # To do linear combinations in gls

#if (!require("emmeans")) install.packages("emmeans")
#library(emmeans) # To get model-implied means

if (!require("lmerTest")) install.packages("lmerTest")
library(lmerTest) # To get Satterthwaite DDF in lmer

if (!require("performance")) install.packages("performance")
library(performance) # To get ICC in lmer

if (!require("prediction")) install.packages("prediction")
library(prediction) # To get predicted values like Stata does

if (!require("lavaan")) install.packages("lavaan")
library(prediction) # To fit path models and SEMs 


################################################################################
#####           BEGIN DATA MANIPULATION OF CHAPTER 10a MODELS              #####
#####            CHANGE "filesave" TO YOUR DIRECTORY                       #####
################################################################################

# Define variables for working directory and data name
filesave = "C:\\Dropbox/23_PSQF7375_AdvLong/PSQF7375_AdvLong_Example2/"
filename = "SAS_Chapter10a.sas7bdat"
setwd(dir=filesave)

# Import chapter 10a stacked data with labels
Example2 = read_sas(data_file=paste0(filesave,filename)) 
# Convert to data frame as data frame without labels to use for analysis
Example2 = as.data.frame(Example2)

# Create centered predictors for analysis
# Time in study
Example2$time=Example2$tvage-Example2$ageT0
Example2$timesq=Example2$time*Example2$time
# Fixing 1 case rounded to 9
Example2$occasion[which(Example2$occasion==9)]=8
# Age (years since birth) variables
Example2$roundage=round(Example2$tvage,digits=0)
# Fixing 2 cases above 95
Example2$roundage[which(Example2$roundage>95)]=95
Example2$tvage84=Example2$tvage-84
Example2$tvage84sq=Example2$tvage84*Example2$tvage84
Example2$ageT084=Example2$ageT0-84
Example2$ageT084sq=Example2$ageT084*Example2$ageT084

############## Labels as comments only #########################
#time      = "time: Years since Time 0"
#timesq    = "timesq: Squared Years since Time 0"
#roundage  = "roundage: Age Rounded to Nearest Year"
#tvage84   = "tvage84: Time-Varying Age (0=84 years)"
#tvage84sq = "tvage84sq: Squared Time-Varying Age (0=84 years)"
#ageT084   = "ageT084: Age at Time 0 (0=84 years)"
#ageT084sq = "ageT084sq: Squared Age at Time 0 (0=84 years)"
#################################################################

# Subset sample to complete cases for all predictors
Example2 = Example2[complete.cases(Example2[ , c("tvage","ageT0","recall")]),]


###################################################################################
#######      BEGIN DESCRIPTION VIA EMPTY AND SATURATED MEANS MODELS         #######
###################################################################################

# Open external file to save results to
txtStart(file=paste0(filesave,"AdvLong_Example2_R_Output.txt"))

print("Descriptive Statistics")
 describe(x=Example2[ , c("ageT0","tvage","time","recall")])
corr.test(x=Example2[ , c("ageT0","tvage","time","recall")])

txtComment(" ") # insert blank space in output

print("Empty Means, Random Intercept Model for Age")
Age = lmer(data=Example2, REML=FALSE, formula=tvage~1+(1|PersonID))
summary(Age); icc(Age)
print("Does the random intercept improve model fit?")
ranova(Age, reduce.term=TRUE) # LRT for removing random intercept

txtComment(" ") # insert blank space in output

print("Empty Means, Random Intercept Model for Time")
Time = lmer(data=Example2, REML=FALSE, formula=time~1+(1|PersonID))
summary(Time); icc(Time)
print("Does the random intercept improve model fit?")
ranova(Time, reduce.term=TRUE) # LRT for removing random intercept

txtComment(" ") # insert blank space in output

print("Model 0: Empty Means, Random Intercept Model for Recall Outcome")
Empty = lmer(data=Example2, REML=FALSE, formula=recall~1+(1|PersonID))
llikAIC(Empty); summary(Empty); icc(Empty)
print("Does the random intercept improve model fit?")
ranova(Empty, reduce.term=TRUE) # LRT for removing random intercept

txtComment(" ") # insert blank space in output


###########################################################################################
print("Saturated Means by Rounded Age, Random Intercept Model")
SatAge = lmer(data=Example2, REML=FALSE, formula=recall~0+as.factor(roundage)+(1|PersonID))
summary(SatAge)

# Save and convert list output of means into data frame, compute CIs
SumSatAge = summary(SatAge)
AgeMeans = data.frame(SumSatAge$coefficients)
AgeMeans$LCI = AgeMeans$Estimate-1.96*AgeMeans$Std..Error
AgeMeans$UCI = AgeMeans$Estimate+1.96*AgeMeans$Std..Error
# Extract age from row label from character 20-21
AgeMeans$age = as.numeric(substr(x=rownames(AgeMeans), start=20, stop=21))
AgeMeans # Show result in output

# Save plot: open file, make plot, close file
png(file="R Plots/R Recall by Age.png")  # open file
plot(x=AgeMeans$age, y=AgeMeans$Estimate, type="l",
     ylim=c(min(AgeMeans$LCI), max(AgeMeans$UCI)),
     xlab="Rounded Age in Years", ylab="Predicted Recall")
# Loop over points to add error bars
estimate=1
for (estimate in 1:nrow(AgeMeans)){
  points(x =   AgeMeans$age[estimate], y=AgeMeans$Estimate[estimate])
   lines(x = c(AgeMeans$age[estimate], AgeMeans$age[estimate]), 
         y = c(AgeMeans$LCI[estimate], AgeMeans$UCI[estimate]))
}; dev.off()  # close file

txtComment(" ") # insert blank space in output

###########################################################################################
print("Saturated Means by Rounded Occasion, Random Intercept Model")
SatTim = lmer(data=Example2, REML=FALSE, formula=recall~0+as.factor(occasion)+(1|PersonID))
summary(SatTim)

# Save and convert list output of means into data frame, compute CIs
SumSatTim = summary(SatTim)
TimMeans = data.frame(SumSatTim$coefficients)
TimMeans$LCI = TimMeans$Estimate-1.96*TimMeans$Std..Error
TimMeans$UCI = TimMeans$Estimate+1.96*TimMeans$Std..Error
# Extract occasion from row label from character 20-20
TimMeans$Tim = as.numeric(substr(x=rownames(TimMeans), start=20, stop=20))
TimMeans # Show result in output

# Save plot: open file, make plot, close file
png(file="R Plots/R Recall by Time.png")  # open file
plot(x=TimMeans$Tim, y=TimMeans$Estimate, type="l",
     ylim=c(min(TimMeans$LCI), max(TimMeans$UCI)),
     xlab="Rounded Time in Years", ylab="Predicted Recall")
# Loop over points to add error bars
estimate=1
for (estimate in 1:nrow(TimMeans)){
  points(x =   TimMeans$Tim[estimate], y=TimMeans$Estimate[estimate])
  lines(x = c(TimMeans$Tim[estimate], TimMeans$Tim[estimate]), 
        y = c(TimMeans$LCI[estimate], TimMeans$UCI[estimate]))
}; dev.off()  # close file

txtComment(" ") # insert blank space in output


###################################################################################
#######             BEGIN AGE-AS-TIME AND TIME-AS-TIME MODELS               #######
###################################################################################

print("Model 1a Age: Fixed Quadratic, Random Intercept Model")
RIAge = lmer(data=Example2, REML=FALSE, formula=recall~1+tvage84+tvage84sq+(1|PersonID))
llikAIC(RIAge); summary(RIAge)

print("Total R2 for fixed age slopes")
Example2$PredRIAge = predict(RIAge, re.form=NA)
rRIAge = cor.test(Example2$PredRIAge, Example2$recall, method="pearson")
rRIAge$estimate^2

txtComment(" ") # insert blank space in output

###########################################################################################
print("Model 1b Time: Fixed Quadratic, Random Intercept Model")
RITim = lmer(data=Example2, REML=FALSE, formula=recall~1+time+timesq+(1|PersonID))
llikAIC(RITim); summary(RITim)

print("Total R2 for fixed time slopes")
Example2$PredRITim = predict(RITim, re.form=NA)
rRITim = cor.test(Example2$PredRITim, Example2$recall, method="pearson")
rRITim$estimate^2

txtComment(" ") # insert blank space in output

###########################################################################################
print("Model 2a Age: Fixed Quadratic, Random Intercept Model")
print("Controlling for Birth Cohort as Contextual Effects")
RICohAge = lmer(data=Example2, REML=FALSE, formula=recall~1+tvage84+tvage84sq
                +ageT084+ageT084sq+tvage84:ageT084+(1|PersonID))
llikAIC(RICohAge); summary(RICohAge) 
print("Total Linear    Birth Cohort on Intercept"); contest1D(RICohAge, L=c(0,1,0,1,0,0)) 
print("Total Quadratic Birth Cohort on Intercept"); contest1D(RICohAge, L=c(0,0,1,0,1,1)) 
print("Total Linear Birth Cohort on Linear Slope"); contest1D(RICohAge, L=c(0,0,2,0,0,1)) 
print("LRT for birth cohort contextual fixed slopes"); anova(RICohAge,RIAge)

print("Total R2 for all fixed slopes and change in total R2 for birth cohort effects")
Example2$PredRICohAge = predict(RICohAge, re.form=NA)
rRICohAge = cor.test(Example2$PredRICohAge, Example2$recall, method="pearson")
rRICohAge$estimate^2; rRICohAge$estimate^2-rRIAge$estimate^2

txtComment(" ") # insert blank space in output

###########################################################################################
print("Model 2b Time: Fixed Quadratic, Random Intercept Model")
print("Controlling for Birth Cohort as Total Effects")
RICohTim = lmer(data=Example2, REML=FALSE, formula=recall~1+time+timesq
                +ageT084+ageT084sq+time:ageT084+(1|PersonID))
llikAIC(RICohTim); summary(RICohTim)
print("Contextual Linear    Birth Cohort on Intercept"); contest1D(RICohTim, L=c(0,-1, 0,1,0, 0)) 
print("Contextual Quadratic Birth Cohort on Intercept"); contest1D(RICohTim, L=c(0, 0, 1,0,1,-1)) 
print("Contextual Linear Birth Cohort on Linear Slope"); contest1D(RICohTim, L=c(0, 0,-2,0,0, 1)) 
print("LRT for birth cohort total fixed slopes"); anova(RICohTim,RITim)

print("Total R2 for all fixed slopes and change in total R2 for age effects")
Example2$PredRICohTim = predict(RICohTim, re.form=NA)
rRICohTim = cor.test(Example2$PredRICohTim, Example2$recall, method="pearson")
rRICohTim$estimate^2; rRICohTim$estimate^2-rRITim$estimate^2

txtComment(" ") # insert blank space in output

###########################################################################################
print("Model 3a Age: Add Random Linear TVage to Model 2a")
RLCohAge = lmer(data=Example2, REML=FALSE, formula=recall~1+tvage84+tvage84sq
                +ageT084+ageT084sq+tvage84:ageT084+(1+tvage84|PersonID))
llikAIC(RLCohAge); summary(RLCohAge)
print("LRT for random linear TVage slope"); anova(RLCohAge,RICohAge)

txtComment(" ") # insert blank space in output

###########################################################################################
print("Model 3b Time: Add Random Linear Time to Model 2b")
RLCohTim = lmer(data=Example2, REML=FALSE, formula=recall~1+time+timesq
                +ageT084+ageT084sq+time:ageT084+(1+time|PersonID))
llikAIC(RLCohTim); summary(RLCohTim)
print("LRT for random linear time slope"); anova(RLCohTim,RICohTim)

txtComment(" ") # insert blank space in output

###########################################################################################
print("Model 4a Age: Add Random Linear AgeCoh to Model 3a -- won't run")
RL2CohAge = lmer(data=Example2, REML=FALSE, formula=recall~1+tvage84+tvage84sq
                 +ageT084+ageT084sq+tvage84:ageT084+(1+tvage84+ageT084|PersonID))
#llikAIC(RL2CohAge); summary(RL2CohAge)
#print("LRT for random baseline age slope"); anova(RL2CohAge,RLCohAge)

txtComment(" ") # insert blank space in output

###########################################################################################
print("Model 4b Time: Add Random Linear Time to Model 3b -- won't run")
RL2CohTim = lmer(data=Example2, REML=FALSE, formula=recall~1+time+timesq
                 +ageT084+ageT084sq+time:ageT084+(1+time+ageT084|PersonID))
#llikAIC(RL2CohTim); summary(RL2CohTim)
#print("LRT for random baseline age slope"); anova(RL2CohTim,RLCohTim)

txtComment(" ") # insert blank space in output

###########################################################################################
# Close external file of saved results
txtStop()



