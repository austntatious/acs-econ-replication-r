#==============================================================================
#   Winters (2016) Replication
#==============================================================================
# First version by Matt Holian 2/22/2019
# This version 2/23/2019

# This file reproduces the statistics reported in:
  
  # John V. Winters (2016) Is economics a good major for future lawyers? 
  # Evidence from earnings data, The Journal of Economic Education, 47:2, 187-191.

# The CPI variable code below was written by John V. Winters.

# I base the format of this script off of tutorials by Bill Sundstrom and Michael Kevane; 
# see their Guide to R at: https://rpubs.com/wsundstrom/home

#==============================================================================
#   1. Settings, packages, and options
#==============================================================================

# You should generally run all of the commands in this Section 1 

# Clear the working space
rm(list = ls())

# Set working directory (edit for YOUR folder)

setwd("~/Desktop/matt-holian")

# Load the packages; you may not need them all but it's better than not loading a package you need
# In the code below: ggplot2 is for plotting (not used)
# stargazer is for creating tables of reg results
# sandwich is for robust standard errors (not used)
# car is for recoding; plyr contains the function "count"

library(ggplot2)
library(stargazer)
library(sandwich)
library(car)
library(plyr)
library(survey)
library(srvyr)
library(ipumsr)
library(spatstat)

# turn off scientific notation except for big numbers
options(scipen = 9)
# function to calculate corrected SEs for regression; this isn't really needed for main tables
cse <- function(reg) {
  rob = sqrt(diag(vcovHC(reg, type = "HC1")))
  return(rob)
}

#==============================================================================
#   2. Data section
#==============================================================================

# after downloading CSV file from IPUMS I read it in as acsMajor, save it as a RData file,
# and comment out the line as I will not be reading in the CSV file again.

# acsMajor = read.csv("usa_00013.csv",header=TRUE,sep=",")
# save(acsMajor, file = "acsFullExtract.RData")

# after the RData file is saved to my directory, I can work with it:

#load("acsFullExtract.RData")

# But it is very large so instead I created a subset1w
# This is the estimation subsamples for Winters (2016) replication; I save it as a CSV and RData files.

#subset1w = subset(acsMajor, OCC1990==178 & EDUCD>114 & AGE>24 & AGE<62)
#save(subset1w, file="subset1w.RData")
#write.csv(subset1w, file = "subset1w.csv")

load("subset1w.RData")

# generating econ and history major variables based on Winters (2016)

subset1w$econ=recode(subset1w$DEGFIELDD, "5501=1; 6205=1 ; else=0")
subset1w$history=recode(subset1w$DEGFIELDD, "6402=1; 6403=1 ; else=0")

# generating two inflation-adjusted income measures  to 2015 dollars
# For the first, see: https://cps.ipums.org/cps/cpi99.shtml
# acsMajor$INCEARNadj= (acsMajor$INCEARN)*(acsMajor$CPI99)*1.430
# The second inflation adjustment is from Winters (2016)

subset1w$cpi=recode(subset1w$YEAR, "2009=214.537; 2010=218.056; 2011=224.939; 2012=229.594; 2013=232.957")
subset1w$INCEARNadj2=(subset1w$INCEARN)*233.707/(subset1w$cpi)


# generating estimation subsample2

subset2w=subset(subset1w, OCC1990==178  & EDUCD>114 & AGE>29 & AGE<62)


#==============================================================================
#   3. Analysis section
#==============================================================================

# frequency table to replicate Winters (2016, Table 1)

y=count(subset1w, 'DEGFIELDD')
y
#write.csv(y, file = "degrees_ddd_tabulate.csv")
#The results are identical to his to two decimal places (except History is slightly off)

# try sum stats table to replicate Winters (2016, Table 2)

stargazer(subset(subset2w, econ==1), type="text", summary.stat=c("n", "mean", "median", "sd", "min", "max" ), digits=2, title="ACS Earnings Major Summary Statistics")
stargazer(subset1, type="text", summary.stat=c("n", "mean", "median", "sd", "min", "max" ), digits=2, title="ACS Earnings Major Summary Statistics")

# This command finds mean earnings are 187,873.9 and median are 139,437.5,
# This is somewhat higher than Winters (2016) who reported 182,359 and 130,723, respectively.
# Try weighting the average using a regression approach

reg1=lm(INCEARNadj2~econ, data=subset2w, weights=subset2w$PERWT)
summary(reg1)

# Adding the coefficients 149708.6+32649.9=182,358.5. This is identical to Winters.
# And chaging econ to history in the code above, 150776.7+10904.8=161,681.5. This is also identical to Winters.
# The dummy variable regression approach works for means but does not produced medians.

# Weighted Arithmetic Mean

mean_econ=weighted.mean(subset(subset2w, econ==1)$INCEARNadj2, subset(subset2w, econ==1)$PERWT, na.rm = FALSE)

# Weighted Arithmetic Mean

med_econ=weighted.median(subset(subset2w, econ==1)$INCEARNadj2, subset(subset2w, econ==1)$PERWT, na.rm = FALSE)

# Now copy code aove and change econ to history and then do the same for the other degree codes in the top 25 majors

mean_hist=weighted.mean(subset(subset2w, history==1)$INCEARNadj2, subset(subset2w, history==1)$PERWT, na.rm = FALSE)
med_hist=weighted.median(subset(subset2w, history==1)$INCEARNadj2, subset(subset2w, history==1)$PERWT, na.rm = FALSE)

#5506	Political Science and Government

mean_pols=weighted.mean(subset(subset2w, DEGFIELDD==5506)$INCEARNadj2, subset(subset2w, DEGFIELDD==5506)$PERWT, na.rm = FALSE)
med_pols=weighted.median(subset(subset2w, DEGFIELDD==5506)$INCEARNadj2, subset(subset2w, DEGFIELDD==5506)$PERWT, na.rm = FALSE)

#3301	English Language and Literature

mean_english=weighted.mean(subset(subset2w, DEGFIELDD==3301)$INCEARNadj2, subset(subset2w, DEGFIELDD==3301)$PERWT, na.rm = FALSE)
med_english=weighted.median(subset(subset2w, DEGFIELDD==3301)$INCEARNadj2, subset(subset2w, DEGFIELDD==3301)$PERWT, na.rm = FALSE)

#5200	Psychology

mean_psyc=weighted.mean(subset(subset2w, DEGFIELDD==5200)$INCEARNadj2, subset(subset2w, DEGFIELDD==5200)$PERWT, na.rm = FALSE)
med_psyc=weighted.median(subset(subset2w, DEGFIELDD==5200)$INCEARNadj2, subset(subset2w, DEGFIELDD==5200)$PERWT, na.rm = FALSE)

#6203	Business Management and Administration

mean_bus=weighted.mean(subset(subset2w, DEGFIELDD==6203)$INCEARNadj2, subset(subset2w, DEGFIELDD==6203)$PERWT, na.rm = FALSE)
med_bus=weighted.median(subset(subset2w, DEGFIELDD==6203)$INCEARNadj2, subset(subset2w, DEGFIELDD==6203)$PERWT, na.rm = FALSE)

#6201	Accounting

mean_acct=weighted.mean(subset(subset2w, DEGFIELDD==6201)$INCEARNadj2, subset(subset2w, DEGFIELDD==6201)$PERWT, na.rm = FALSE)
med_acct=weighted.median(subset(subset2w, DEGFIELDD==6201)$INCEARNadj2, subset(subset2w, DEGFIELDD==6201)$PERWT, na.rm = FALSE)

#6200	General Business

mean_gbus=weighted.mean(subset(subset2w, DEGFIELDD==6200)$INCEARNadj2, subset(subset2w, DEGFIELDD==6200)$PERWT, na.rm = FALSE)
med_gbus=weighted.median(subset(subset2w, DEGFIELDD==6200)$INCEARNadj2, subset(subset2w, DEGFIELDD==6200)$PERWT, na.rm = FALSE)

#4801	Philosophy and Religious Studies

mean_phil=weighted.mean(subset(subset2w, DEGFIELDD==4801)$INCEARNadj2, subset(subset2w, DEGFIELDD==4801)$PERWT, na.rm = FALSE)
med_phil=weighted.median(subset(subset2w, DEGFIELDD==4801)$INCEARNadj2, subset(subset2w, DEGFIELDD==4801)$PERWT, na.rm = FALSE)

#6207	Finance

mean_fin=weighted.mean(subset(subset2w, DEGFIELDD==6207)$INCEARNadj2, subset(subset2w, DEGFIELDD==6207)$PERWT, na.rm = FALSE)
med_fin=weighted.median(subset(subset2w, DEGFIELDD==6207)$INCEARNadj2, subset(subset2w, DEGFIELDD==6207)$PERWT, na.rm = FALSE)

#5301	Criminal Justice and Fire Protection

mean_crim=weighted.mean(subset(subset2w, DEGFIELDD==5301)$INCEARNadj2, subset(subset2w, DEGFIELDD==5301)$PERWT, na.rm = FALSE)
med_crim=weighted.median(subset(subset2w, DEGFIELDD==5301)$INCEARNadj2, subset(subset2w, DEGFIELDD==5301)$PERWT, na.rm = FALSE)

#5507	Sociology

mean_soc=weighted.mean(subset(subset2w, DEGFIELDD==5507)$INCEARNadj2, subset(subset2w, DEGFIELDD==5507)$PERWT, na.rm = FALSE)
med_soc=weighted.median(subset(subset2w, DEGFIELDD==5507)$INCEARNadj2, subset(subset2w, DEGFIELDD==5507)$PERWT, na.rm = FALSE)

#1901	Communications

mean_comm=weighted.mean(subset(subset2w, DEGFIELDD==1901)$INCEARNadj2, subset(subset2w, DEGFIELDD==1901)$PERWT, na.rm = FALSE)
med_comm=weighted.median(subset(subset2w, DEGFIELDD==1901)$INCEARNadj2, subset(subset2w, DEGFIELDD==1901)$PERWT, na.rm = FALSE)

#3600	Biology

mean_bio=weighted.mean(subset(subset2w, DEGFIELDD==3600)$INCEARNadj2, subset(subset2w, DEGFIELDD==3600)$PERWT, na.rm = FALSE)
med_bio=weighted.median(subset(subset2w, DEGFIELDD==3600)$INCEARNadj2, subset(subset2w, DEGFIELDD==3600)$PERWT, na.rm = FALSE)

#1902	Journalism

mean_journ=weighted.mean(subset(subset2w, DEGFIELDD==1902)$INCEARNadj2, subset(subset2w, DEGFIELDD==1902)$PERWT, na.rm = FALSE)
med_journ=weighted.median(subset(subset2w, DEGFIELDD==1902)$INCEARNadj2, subset(subset2w, DEGFIELDD==1902)$PERWT, na.rm = FALSE)

#3401	Liberal Arts

mean_libart=weighted.mean(subset(subset2w, DEGFIELDD==3401)$INCEARNadj2, subset(subset2w, DEGFIELDD==3401)$PERWT, na.rm = FALSE)
med_libart=weighted.median(subset(subset2w, DEGFIELDD==3401)$INCEARNadj2, subset(subset2w, DEGFIELDD==3401)$PERWT, na.rm = FALSE)

#2602	French, German, Latin and Other Common Foreign Language Studies

mean_french=weighted.mean(subset(subset2w, DEGFIELDD==2602)$INCEARNadj2, subset(subset2w, DEGFIELDD==2602)$PERWT, na.rm = FALSE)
med_french=weighted.median(subset(subset2w, DEGFIELDD==2602)$INCEARNadj2, subset(subset2w, DEGFIELDD==2602)$PERWT, na.rm = FALSE)

#5505	International Relations

mean_ibus=weighted.mean(subset(subset2w, DEGFIELDD==5505)$INCEARNadj2, subset(subset2w, DEGFIELDD==5505)$PERWT, na.rm = FALSE)
med_ibus=weighted.median(subset(subset2w, DEGFIELDD==5505)$INCEARNadj2, subset(subset2w, DEGFIELDD==5505)$PERWT, na.rm = FALSE)

#1501	Area, Ethnic, and Civilization Studies

mean_ethnic=weighted.mean(subset(subset2w, DEGFIELDD==1501)$INCEARNadj2, subset(subset2w, DEGFIELDD==1501)$PERWT, na.rm = FALSE)
med_ethnic=weighted.median(subset(subset2w, DEGFIELDD==1501)$INCEARNadj2, subset(subset2w, DEGFIELDD==1501)$PERWT, na.rm = FALSE)

#3202	Pre-Law and Legal Studies

mean_prelaw=weighted.mean(subset(subset2w, DEGFIELDD==3202)$INCEARNadj2, subset(subset2w, DEGFIELDD==3202)$PERWT, na.rm = FALSE)
med_prelaw=weighted.median(subset(subset2w, DEGFIELDD==3202)$INCEARNadj2, subset(subset2w, DEGFIELDD==3202)$PERWT, na.rm = FALSE)

#6206	Marketing and Marketing Research

mean_mark=weighted.mean(subset(subset2w, DEGFIELDD==6206)$INCEARNadj2, subset(subset2w, DEGFIELDD==6206)$PERWT, na.rm = FALSE)
med_mark=weighted.median(subset(subset2w, DEGFIELDD==6206)$INCEARNadj2, subset(subset2w, DEGFIELDD==6206)$PERWT, na.rm = FALSE)

#2408	Electrical Engineering

mean_ee=weighted.mean(subset(subset2w, DEGFIELDD==2408)$INCEARNadj2, subset(subset2w, DEGFIELDD==2408)$PERWT, na.rm = FALSE)
med_ee=weighted.median(subset(subset2w, DEGFIELDD==2408)$INCEARNadj2, subset(subset2w, DEGFIELDD==2408)$PERWT, na.rm = FALSE)

#5003	Chemistry

mean_chem=weighted.mean(subset(subset2w, DEGFIELDD==5003)$INCEARNadj2, subset(subset2w, DEGFIELDD==5003)$PERWT, na.rm = FALSE)
med_chem=weighted.median(subset(subset2w, DEGFIELDD==5003)$INCEARNadj2, subset(subset2w, DEGFIELDD==5003)$PERWT, na.rm = FALSE)

#3700	Mathematics

mean_math=weighted.mean(subset(subset2w, DEGFIELDD==3700)$INCEARNadj2, subset(subset2w, DEGFIELDD==3700)$PERWT, na.rm = FALSE)
med_math=weighted.median(subset(subset2w, DEGFIELDD==3700)$INCEARNadj2, subset(subset2w, DEGFIELDD==3700)$PERWT, na.rm = FALSE)

#5502	Anthropology and Archeology

mean_anth=weighted.mean(subset(subset2w, DEGFIELDD==5502)$INCEARNadj2, subset(subset2w, DEGFIELDD==5502)$PERWT, na.rm = FALSE)
med_anth=weighted.median(subset(subset2w, DEGFIELDD==5502)$INCEARNadj2, subset(subset2w, DEGFIELDD==5502)$PERWT, na.rm = FALSE)



#6402	History - see above, combined major codes
#5501	Economics - see above, combined major codes


# Overall I exactly replicated 9/25: econ, history, pols, gbus, phil, bio, comm, psyc, prelaw
# The rest were all very close (within < 1%) but were not exact
# all weighted means were exactly identical




