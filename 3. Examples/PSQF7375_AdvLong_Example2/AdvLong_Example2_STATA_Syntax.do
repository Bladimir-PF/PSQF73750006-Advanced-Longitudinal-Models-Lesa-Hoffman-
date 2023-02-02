// Prevent "more" messages from appearing
set more off
// Control line length
set linesize 150

*********************************************************************************
******  BEGIN DATA MANIPULATION FOR CHAPTER 10a ALTERNATIVE TIME MODELS   *******
******               CHANGE "filesave" to your directory                  *******
*********************************************************************************

// Defining global variable for file location to be replaced in code below
// \\Client\ precedes path in Virtual Desktop outside H drive
     global filesave "C:\Dropbox\23_PSQF7375_AdvLong\PSQF7375_AdvLong_Example2"

// Import chapter 10a stacked data and create centered predictors for analysis
   use "$filesave\STATA_Chapter10a.dta", clear
// Time in study
   gen time = tvage-aget0
   gen timesq = time*time
// Fixing 1 case rounded to 9
   replace occasion=8 if (occasion==9)
// Age (years since birth) variables
   gen roundage = round(tvage,1)
// Fixing 2 cases above 95
   replace roundage=95 if (roundage==97)
   replace roundage=95 if (roundage==100)
   gen tvage84 = tvage-84
   gen tvage84sq = tvage84*tvage84
   gen aget084 = aget0-84
   gen aget084sq = aget084*aget084
   label variable time         "time: Years since Time 0"
   label variable timesq       "timesq: Squared Years since Time 0"
   label variable roundage     "roundage: Age Rounded to Nearest Year"
   label variable tvage84      "tvage84: Time-Varying Age (0=84 years)"
   label variable tvage84sq    "tvage84sq: Squared Time-Varying Age (0=84 years)"
   label variable aget084      "aget084: Age at Time 0 (0=84 years)"
   label variable aget084sq    "aget084: Squared Age at Time 0 (0=84 years)"

// Subset sample to complete cases for all predictors
   egen nummiss = rowmiss(tvage aget0 recall)
   drop if nummiss>0

    
********************************************************************************
******    BEGIN DESCRIPTION VIA EMPTY AND SATURATED MEANS MODELS         *******
********************************************************************************

// Open external file to save results to
   log using $filesave\AdvLong_Example2_STATA_Output.log, replace

display "Descriptive Statistics"
summarize aget0 tvage time recall
pwcorr aget0 tvage time recall, sig

display "Empty Means, Random Intercept Model for Age"
mixed tvage , || personid: , mle nolog 
estat icc  // Intraclass correlation
          
display "Empty Means, Random Intercept Model for Time"
mixed time , || personid: , mle nolog
estat icc  // Intraclass correlation

display "Model 0: Empty Means, Random Intercept Model for Recall Outcome"
mixed recall , || personid: , mle nolog
display "-2LL = " e(ll)*-2  // Print -2LL for model  
estat icc  // Intraclass correlation
          
display "Saturated Means by Rounded Age, Random Intercept Model"
mixed recall i.roundage, || personid: , mle nolog
margins  i.roundage  // get saturated means per age and plot them
marginsplot, xdimension(roundage) name(by_age, replace) 
graph export "$filesave\STATA plots\STATA Recall by Age.png", replace

display "Saturated Means by Rounded Time, Random Intercept Model"
mixed recall i.occasion, || personid: , mle nolog
margins  i.occasion  // get saturated means per occasion and plot them
marginsplot, xdimension(occasion) name(by_time, replace) 
graph export "$filesave\STATA plots\STATA Recall by Time.png", replace

          
********************************************************************************
******         BEGIN AGE-AS-TIME AND TIME-AS-TIME MODELS                 *******
********************************************************************************

display "Model 1a Age: Fixed Quadratic, Random Intercept Model"
mixed recall c.tvage84 c.tvage84sq, || personid: , mle nolog 
display "-2LL = " e(ll)*-2  // Print -2LL for model  
estat ic, n(207)            // Information criteria at N=# persons
estimates store FitRIAge    // Save for LRT
predict PredAge, xb         // Save fixed-pred outcomes for total-R2
corr recall PredAge         // Get total r to make R2
     display r(rho)^2       // Print total R2 relative to empty model


display "Model 1b Time: Fixed Quadratic, Random Intercept Model"
mixed recall c.time c.timesq, || personid: , mle nolog 
display "-2LL = " e(ll)*-2  // Print -2LL for model  
estat ic, n(207)            // Information criteria at N=# persons
estimates store FitRITim    // Save for LRT
predict PredTim, xb         // Save fixed-pred outcomes for total-R2
corr recall PredTim         // Get total r to make R2
     display r(rho)^2       // Print total R2 relative to empty model
     

display "Model 2a Age: Fixed Quadratic, Random Intercept Model"
display "Controlling for Birth Cohort as Contextual Effects"
mixed recall c.tvage84 c.tvage84sq c.aget084 c.aget084sq c.tvage84#c.aget084, ///
             || personid: , mle nolog 
display "-2LL = " e(ll)*-2  // Print -2LL for model  
estat ic, n(207)   // Information criteria at N=# persons
// Total Linear Birth Cohort on Intercept
   lincom c.aget084*1 + c.tvage84*1
// Total Quadratic Birth Cohort on Intercept
   lincom c.aget084sq*1 + c.tvage84#c.aget084*1 + c.tvage84sq*1
// Total Linear Birth Cohort on Linear Slope
   lincom c.tvage84#c.aget084*1 + c.tvage84sq*2
estimates store FitRICohAge     // Save for LRT
lrtest FitRICohAge FitRIAge     // LRT for birth cohort contextual fixed slopes
predict PredCohAge, xb          // Save fixed-pred outcomes for total-R2
corr recall PredCohAge          // Get total r to make R2
     display r(rho)^2           // Print total R2 relative to empty model
          

display "Model 2b Time: Fixed Quadratic, Random Intercept Model"
display "Controlling for Birth Cohort as Total Effects"
mixed recall time c.timesq c.aget084 c.aget084sq c.time#c.aget084, ///
             || personid: , mle nolog
display "-2LL = " e(ll)*-2  // Print -2LL for model  
estat ic, n(207)            // Information criteria at N=# persons
// Contextual Linear Birth Cohort on Intercept
   lincom c.aget084*1 + time*-1
// Contextual Quadratic Birth Cohort on Intercept
   lincom c.aget084sq*1 + c.time#c.aget084*-1 + c.timesq*1
// Contextual Linear Birth Cohort on Linear Slope
   lincom c.time#c.aget084*1 + c.timesq*-2
estimates store FitRICohTim     // Save for LRT
lrtest FitRICohTim FitRITim     // LRT for birth cohort total fixed slopes
predict PredCohTim, xb          // Save fixed-pred outcomes for total R2
corr recall PredCohTim          // Get total r to make R2
     display r(rho)^2           // Print total R2 relative to empty model
          

display "Model 3a Age: Add Random Linear TVage to Model 2a"
mixed recall c.tvage84 c.tvage84sq c.aget084 c.aget084sq c.tvage84#c.aget084, ///
             || personid: tvage84, mle nolog covariance(unstructured)
estat recovariance, relevel(personid) correlation  // GCORR matrix
display "-2LL = " e(ll)*-2      // Print -2LL for model  
estat ic, n(207)                // Information criteria at N=# persons
estimates store FitRLCohAge     // Save for LRT
lrtest FitRLCohAge FitRICohAge  // LRT for random linear TVage slope       


display "Model 3b Time: Add Random Linear Time to Model 2b"
mixed recall c.time c.timesq c.aget084 c.aget084sq c.tvage84#c.aget084, ///
             || personid: time, mle nolog covariance(unstructured)
estat recovariance, relevel(personid) correlation  // GCORR matrix
display "-2LL = " e(ll)*-2      // Print -2LL for model  
estat ic, n(207)                // Information criteria at N=# persons
estimates store FitRLCohTim     // Save for LRT
lrtest FitRLCohTim FitRICohTim  // LRT for random linear time slope
              

display "Model 4a Age: Add Random Linear AgeCoh to Model 3a -- extra iterations"
mixed recall c.tvage84 c.tvage84sq c.aget084 c.aget084sq c.tvage84#c.aget084, ///
             || personid: tvage84 aget084, mle nolog emiterate(100) covariance(unstructured)
estat recovariance, relevel(personid) correlation  // GCORR matrix
display "-2LL = " e(ll)*-2         // Print -2LL for model  
estat ic, n(207)                   // Information criteria at N=# persons
// Gives error message for LRT
//estimates store FitRL2CohAge     // Save for LRT
//lrtest FitRL2CohAge FitRLCohAge  // LRT for random linear baseline age slope        
          
          
display "Model 4b Time: Add Random Linear AgeCoh to Model 3b -- extra iterations"
mixed recall c.time c.timesq c.aget084 c.aget084sq c.tvage84#c.aget084, ///
             || personid: time aget084, mle nolog emiterate(100) covariance(unstructured)
estat recovariance, relevel(personid) correlation  // GCORR matrix
display "-2LL = " e(ll)*-2       // Print -2LL for model  
estat ic, n(207)                 // Information criteria at N=# persons
estimates store FitRL2CohTim     // Save for LRT
lrtest FitRL2CohTim FitRLCohTim  // LRT for random linear baseline age slope                              
          
          
// Close external file of saved results
   log close

