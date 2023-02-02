* Output options: all can be turned on/off by adding or removing the NO;
* page number, date, centering, or page breaks, page length and width;
OPTIONS NOnumber NOdate NOcenter FormDlim=' ' PageSize=MAX LineSize=MAX;
* Eliminate SAS default titles and names of tables in output (TRACE ON to show);
TITLE; ODS TRACE OFF; ODS LISTING CLOSE;

***********************************************************************************;
*******   MACRO PROGRAMS TO AUTOMATE CALCULATIONS FOR CHAPTER 10a MODELS    *******;
*******               NOTHING IN HERE NEEDS TO BE CHANGED                   *******;
***********************************************************************************;

* To use FitTest macro;
* FitFewer = Name of ODS InfoCrit table for nested model;
* FitMore  = Name of ODS InfoCrit table for comparison model;
%MACRO FitTest(FitFewer=,FitMore=);
DATA &FitFewer.; LENGTH Name $30.; SET &FitFewer.; Name="&FitFewer."; RUN;
DATA &FitMore.;  LENGTH Name $30.; SET &FitMore.;  Name="&FitMore.";  RUN;
DATA FitCompare; LENGTH Name $30.; SET &FitFewer. &FitMore.; RUN;
DATA FitCompare; SET FitCompare; DevDiff=Lag1(Neg2LogLike)-Neg2LogLike;
     DFdiff=Parms-LAG1(Parms); Pvalue=1-PROBCHI(DevDiff,DFdiff);
     DROP AICC HQIC CAIC; RUN;
TITLE9 "Likelihood Ratio Test for &FitFewer. vs. &FitMore.";
PROC PRINT NOOBS DATA=FitCompare; RUN; TITLE9;
%MEND FitTest;

* To use TotalR2 macro;
* DV =        Case-sensitive name of dependent variable;
* PredFewer = Name of OUTPM= data file of predicted outcomes for nested model;
* PredMore =  Name of OUTPM= data file of predicted outcomes for comparison model;
%MACRO TotalR2(DV=,PredFewer=,PredMore=);
PROC CORR NOPRINT NOSIMPLE DATA=&PredFewer. OUTP=CorrFewer; VAR pred &DV.; RUN;
PROC CORR NOPRINT NOSIMPLE DATA=&PredMore.  OUTP=CorrMore;  VAR pred &DV.; RUN;
DATA CorrFewer; LENGTH Name $30.; SET CorrFewer; Name="&PredFewer."; RUN;
DATA CorrMore;  LENGTH Name $30.; SET CorrMore;  Name="&PredMore.";  RUN;
DATA CorrCompare; LENGTH Name $30.; SET CorrFewer CorrMore; 
     PredCorr=Pred; TotalR2=PredCorr*PredCorr; 
     IF _NAME_="Pred" OR MISSING(_NAME_)=1 THEN DELETE; DROP Pred; RUN;
DATA CorrCompare; SET CorrCompare; TotalR2Diff=TotalR2-LAG1(TotalR2);
     KEEP Name PredCorr TotalR2 TotalR2Diff; RUN; 
TITLE9 "Total R2 (% Reduction) for &PredFewer. vs. &PredMore.";
PROC PRINT NOOBS DATA=CorrCompare; RUN; TITLE9; 
%MEND TotalR2;

* To use PseudoR2 macro;
* Ncov =     TOTAL # entries in covariance parameter estimates table;
* CovFewer = Name of ODS CovParms table for nested model;
* CovMore =  Name of ODS CovParms table for comparison model;
%MACRO PseudoR2(NCov=,CovFewer=,CovMore=);
DATA &CovFewer.; LENGTH Name $30.; SET &CovFewer.; Name="&CovFewer."; RUN;
DATA &CovMore.;  LENGTH Name $30.; SET &CovMore.;  Name="&CovMore.";  RUN;
DATA CovCompare; LENGTH Name $30.; SET &CovFewer. &CovMore.; RUN;
DATA CovCompare; SET CovCompare; 
     PseudoR2=(LAG&Ncov.(Estimate)-Estimate)/LAG&Ncov.(Estimate); RUN;
DATA CovCompare; SET CovCompare; 
     IF CovParm IN("UN(2,1)","UN(3,1)","UN(4,1)","UN(3,2)","UN(4,2)","UN(4,3)") 
     THEN DELETE; RUN;
TITLE9 "PsuedoR2 (% Reduction) for &CovFewer. vs. &CovMore.";
PROC PRINT NOOBS DATA=CovCompare; RUN; TITLE9;
%MEND PseudoR2;


***********************************************************************************;
*******  BEGIN DATA MANIPULATION FOR CHAPTER 10a ALTERNATIVE TIME MODELS    *******;
*******                 CHANGE "filesave" to your directory                 *******;
***********************************************************************************;

* Defining global variable for file location to be replaced in code below;
%LET filesave= C:\Dropbox\23_PSQF7375_AdvLong\PSQF7375_AdvLong_Example2;
* Location for SAS files for these models (uses macro variable filesave);
LIBNAME filesave "&filesave.";

* Import chapter 10a stacked data into work library;
DATA work.Example2; SET filesave.SAS_Chapter10a; RUN;

* Create centered predictors for analysis;
DATA work.Example2; SET work.Example2;
* Time in study;
  time = tvage-ageT0;
  timesq = time*time;
* Fixing 1 case rounded to 9;
  IF occasion=9 THEN occasion=8;
* Age (years since birth) variables;
  roundage = ROUND(tvage,1);
* Fixing 2 cases above 95;
  IF roundage>95 THEN roundage=95;
  tvage84 = tvage-84;
  tvage84sq = tvage84*tvage84;
  ageT084 = ageT0-84;
  ageT084sq = ageT084*ageT084;
LABEL
  time      = "time: Years since Time 0"
  timesq    = "timesq: Squared Years since Time 0"
  roundage  = "roundage: Age Rounded to Nearest Year"
  tvage84   = "tvage84: Time-Varying Age (0=84 years)"
  tvage84sq = "tvage84sq: Squared Time-Varying Age (0=84 years)"
  ageT084   = "ageT084: Age at Time 0 (0=84 years)"
  ageT084sq = "ageT084sq: Squared Age at Time 0 (0=84 years)";
* Subset sample to complete cases for all predictors;
  IF NMISS(tvage, ageT0, recall)>0 THEN DELETE;
RUN; 


***********************************************************************************;
*******      BEGIN DESCRIPTION VIA EMPTY AND SATURATED MEANS MODELS         *******;
***********************************************************************************;

* Open external file to save results to;
ODS RTF FILE="&filesave.\AdvLong_Example2_SAS_Output.rtf" 
        STYLE=HTMLBlue STARTPAGE=NO BODYTITLE; 

TITLE1 "Descriptive Statistics";
PROC MEANS NDEC=4 DATA=work.Example2;
     VAR ageT0 tvage time recall;
RUN; 
PROC CORR NOSIMPLE DATA=work.Example2;
     VAR ageT0 tvage time recall;
RUN; TITLE1;


TITLE1 "Empty Means, Random Intercept Model for Age";
PROC MIXED DATA=work.Example2 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID;
     MODEL tvage =  / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / V VCORR TYPE=UN SUBJECT=PersonID;
RUN; TITLE1;
 
TITLE1 "Empty Means, Random Intercept Model for Time";
PROC MIXED DATA=work.Example2 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID;
     MODEL time =  / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / V VCORR TYPE=UN SUBJECT=PersonID;
RUN; TITLE1;

TITLE1 "Model 0: Empty Means, Random Intercept Model for Recall Outcome";
PROC MIXED DATA=work.Example2 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID;
     MODEL recall =  / SOLUTION CL CHISQ DDFM=Satterthwaite OUTPM=PredEmpty;
     RANDOM INTERCEPT / V VCORR TYPE=UN SUBJECT=PersonID;
     ODS OUTPUT CovParms=CovEmpty; * Save for pseudo-R2;
RUN; TITLE1;
 

TITLE1 "Saturated Means by Rounded Age, Random Intercept Model";
PROC GLIMMIX DATA=work.Example2 NOCLPRINT NAMELEN=100 METHOD=MSPL;
     CLASS PersonID roundage;
     MODEL recall = roundage / LINK=IDENTITY DIST=NORMAL DDFM=Satterthwaite;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=PersonID;
     LSMEANS roundage / PLOT=MEANPLOT(JOIN CLBAND);
RUN; TITLE1;
 
TITLE1 "Saturated Means by Rounded Time, Random Intercept Model";
PROC GLIMMIX DATA=work.Example2 NOCLPRINT NAMELEN=100 METHOD=MSPL;
     CLASS PersonID occasion;
     MODEL recall = occasion / LINK=IDENTITY DIST=NORMAL DDFM=Satterthwaite;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=PersonID;
     LSMEANS occasion / PLOT=MEANPLOT(JOIN CLBAND);
RUN; TITLE1;

 
********************************************************************************;
******         BEGIN AGE-AS-TIME AND TIME-AS-TIME MODELS                 *******;
********************************************************************************;

TITLE1 "Model 1a Age: Fixed Quadratic, Random Intercept Model";
PROC MIXED DATA=work.Example2 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
CLASS PersonID;
MODEL recall = tvage84 tvage84sq / SOLUTION CL CHISQ DDFM=Satterthwaite OUTPM=PredRIAge; * Save for total R2;
RANDOM INTERCEPT / TYPE=UN SUBJECT=PersonID;
ODS OUTPUT InfoCrit=FitRIAge CovParms=CovRIAge; * Save for LRT and pseudo-R2;
RUN; TITLE1;
* Call macro to calculate Pseudo-R2 for overall model;
  %PseudoR2(Ncov=2, CovMore=CovRIAge, CovFewer=CovEmpty);


TITLE1 "Model 1b Time: Fixed Quadratic, Random Intercept Model";
PROC MIXED DATA=work.Example2 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
CLASS PersonID;
MODEL recall = time timesq / SOLUTION CL CHISQ DDFM=Satterthwaite OUTPM=PredRITim; * Save for total R2;
RANDOM INTERCEPT / TYPE=UN SUBJECT=PersonID;
ODS OUTPUT InfoCrit=FitRITim CovParms=CovRITim; * Save for LRT and pseudo-R2;
RUN; TITLE1;
* Call macro to calculate Pseudo-R2 for overall model;
  %PseudoR2(Ncov=2, CovMore=CovRITim, CovFewer=CovEmpty);

 
TITLE1 "Model 2a Age: Fixed Quadratic, Random Intercept Model";
TITLE2 "Controlling for Birth Cohort as Contextual Effects";
PROC MIXED DATA=work.Example2 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
CLASS PersonID;
MODEL recall = tvage84 tvage84sq ageT084 ageT084sq tvage84*ageT084
                / SOLUTION CL CHISQ DDFM=Satterthwaite OUTPM=PredRICohAge; * Save for total R2;
RANDOM INTERCEPT / TYPE=UN SUBJECT=PersonID;
ODS OUTPUT InfoCrit=work.FitRICohAge CovParms=CovRICohAge;  * Save for LRT and pseudo-R2;
ESTIMATE "Total Linear Birth Cohort on Intercept"     ageT084 1 tvage84 1 / CL;
ESTIMATE "Total Quadratic Birth Cohort on Intercept"  ageT084sq 1 tvage84*ageT084 1 tvage84sq 1 / CL;
ESTIMATE "Total Linear Birth Cohort on Linear Slope"  tvage84*ageT084 1 tvage84sq 2 / CL;
RUN; TITLE1; TITLE2;
* Call macro to calculate LRT for birth cohort contextual fixed slopes;
  %FitTest(FitMore=FitRICohAge, FitFewer=FitRIAge);
* Call macro to calculate Total R2 for overall model;
  %TotalR2(DV=recall, PredMore=PredRICohAge, PredFewer=PredRIAge);
* Call macro to calculate Pseudo-R2 for overall model;
  %PseudoR2(Ncov=2, CovMore=CovRICohAge, CovFewer=CovRIAge);


TITLE1 "Model 2b Time: Fixed Quadratic, Random Intercept Model";
TITLE2 "Controlling for Birth Cohort as Total Effects";
PROC MIXED DATA=work.Example2 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
CLASS PersonID;
MODEL recall = time timesq ageT084 ageT084sq time*ageT084
                / SOLUTION CL CHISQ DDFM=Satterthwaite OUTPM=PredRICohTim; * Save for total R2;
RANDOM INTERCEPT / TYPE=UN SUBJECT=PersonID;
ODS OUTPUT InfoCrit=FitRICohTim CovParms=CovRICohTim; * Save for LRT and pseudo-R2;
ESTIMATE "Contextual Linear Birth Cohort on Intercept"     ageT084 1 time -1 / CL;
ESTIMATE "Contextual Quadratic Birth Cohort on Intercept"  ageT084sq 1 time*ageT084 -1 timesq 1 / CL;
ESTIMATE "Contextual Linear Birth Cohort on Linear Slope"  time*ageT084 1 timesq -2 / CL;
RUN; TITLE1; TITLE2;
* Call macro to calculate LRT for birth cohort total fixed slopes;
  %FitTest(FitMore=FitRICohTim, FitFewer=FitRITim);
* Call macro to calculate Total R2 for overall model;
  %TotalR2(DV=recall, PredMore=PredRICohTim, PredFewer=PredRITim);
* Call macro to calculate Pseudo-R2 for overall model;
  %PseudoR2(Ncov=2, CovMore=CovRICohTim, CovFewer=CovRITim);


TITLE1 "Model 3a Age: Add Random Linear TVage to Model 2a";
PROC MIXED DATA=work.Example2 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
CLASS PersonID;
MODEL recall = tvage84 tvage84sq ageT084 ageT084sq tvage84*ageT084
                / SOLUTION CL CHISQ DDFM=Satterthwaite;
RANDOM INTERCEPT tvage84 / GCORR TYPE=UN SUBJECT=PersonID;
ODS OUTPUT InfoCrit=work.FitRLCohAge;  * Save for LRT;
RUN; TITLE1;
* Call macro to calculate LRT for random linear TVage slope;
  %FitTest(FitMore=FitRLCohAge, FitFewer=FitRICohAge);


TITLE1 "Model 3b Time: Add Random Linear Time to Model 2b";
PROC MIXED DATA=work.Example2 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID;
     MODEL recall = time timesq ageT084 ageT084sq time*ageT084
                     / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT time / GCORR TYPE=UN SUBJECT=PersonID;
     ODS OUTPUT InfoCrit=FitRLCohTim;
RUN; TITLE1;
* Call macro to calculate LRT for random linear time slope;
  %FitTest(FitMore=FitRLCohTim, FitFewer=FitRICohTim);


TITLE1 "Model 4a Age: Add Random Linear AgeCoh to Model 3a -- extra iterations";
PROC MIXED DATA=work.Example2 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML MAXITER=500;
CLASS PersonID;
MODEL recall = tvage84 tvage84sq ageT084 ageT084sq tvage84*ageT084
                / SOLUTION CL CHISQ DDFM=Satterthwaite;
RANDOM INTERCEPT tvage84 ageT084 / GCORR TYPE=UN SUBJECT=PersonID;
ODS OUTPUT InfoCrit=work.FitRL2CohAge;  * Save for LRT;
RUN; TITLE1;
* Call macro to calculate LRT for random linear baseline age slope;
  %FitTest(FitMore=FitRL2CohAge, FitFewer=FitRLCohAge);


TITLE1 "Model 4b Time: Add Random Linear AgeCoh to Model 3b -- extra iterations";
PROC MIXED DATA=work.Example2 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML MAXITER=500;
     CLASS PersonID;
     MODEL recall = time timesq ageT084 ageT084sq time*ageT084
                     / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT time ageT084 / GCORR TYPE=UN SUBJECT=PersonID;
     ODS OUTPUT InfoCrit=FitRL2CohTim;
RUN; TITLE1;
* Call macro to calculate LRT for random linear baseline age slope;
  %FitTest(FitMore=FitRL2CohTim, FitFewer=FitRLCohTim);


* Close external file of saved results;
ODS RTF CLOSE;

