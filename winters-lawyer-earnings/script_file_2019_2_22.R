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
setwd("~/Documents/matt-holian")

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
load("acsFullExtract.RData")

# But it is very large so instead I created a subset1w
# This is the estimation subsamples for Winters (2016) replication; I save it as a CSV and RData files.

# create occ subset takes occupation, education level, age bounds, and a string name for the subset to be called
create_occupation_subset <- function(occu_code, education_minimum, age_lower, age_upper) {
  # rename subset to whatever variable is most relevant
  # change filename to something that identifies subset easily - degree,occupation, etc
  
  # todo: find way to dynamically name all filenames automatically
  # this way, passing a filename to the create_occ function will automatically name
  # all Rdata files as the filename parameter 
  masters_lawyers_midage_subset <- 
    subset(acsMajor, OCC1990==occu_code & EDUCD>education_minimum & AGE>age_lower & AGE<age_upper) 
  
  # create Rdata and csv file (are both completely necessary?)
  save(masters_lawyers_midage_subset, file="masters_lawyers_midage_subset.Rdata")
  write.csv(masters_lawyers_midage_subset, file="masters_lawyers_midage_subset.csv")
  
  
  return(masters_lawyers_midage_subset)
}

# occ1990==178 is lawyers, educd>114 is graduate degrees and above
masters_lawyers_midage_subset <- create_occupation_subset(178,114,27,62) 

# load("masters_lawyers_midage_subset.RData")

# generating econ and history major variables based on Winters (2016)
# EDIT: REMOVING THIS CODE SO THAT WE CAN SIMPLY USE DEGFIELD FUNCTION
# masters_lawyers_midage_subset$econ=recode(subset1w$DEGFIELDD, "5501=1; 6205=1 ; else=0")
# masters_lawyers_midage_subset=recode(subset1w$DEGFIELDD, "6402=1; 6403=1 ; else=0")


# generating two inflation-adjusted income measures  to 2015 dollars
# For the first, see: https://cps.ipums.org/cps/cpi99.shtml
# acsMajor$INCEARNadj= (acsMajor$INCEARN)*(acsMajor$CPI99)*1.430
# The second inflation adjustment is from Winters (2016)

# why do we recode cpi?
# add CPI to the column 
subset1w$cpi=recode(subset1w$YEAR, "2009=214.537; 2010=218.056; 2011=224.939; 2012=229.594; 2013=232.957")
subset1w$INCEARNadj2=(subset1w$INCEARN)*233.707/(subset1w$cpi)

# why do we generate estimation subsample 2, or how it diff from sbsmple1 
# generating estimation subsample2

#==============================================================================
#   3. Analysis section
#==============================================================================

# frequency table for top majors to replicate Winters (2016, Table 1)

sort_degfield <- function(s) {
  x = count(s, 'DEGFIELDD')
  sorted_degfield <- x[order(x$freq),]
  return (sorted_degfield)
}

sort_degfield(masters_lawyers_midage_subset)

# print top 20 majors and percentage of total subset 
# replace degfield numbers with matching human readable words and merge
# Sort top 20 majors and output names & percentages 
# take array of top 20 majors and output variables for each major


# write.csv(degfield_counts, file = "degrees_ddd_tabulate.csv")

#The results are identical to his to two decimal places (except History is slightly off)

# try sum stats table to replicate Winters (2016, Table 2)

## TO DO: change stargazer table function to only summarize stats for income var

stargazer(subset(subset2w, econ==1), type="text", summary.stat=c("n", "mean", "median", "sd", "min", "max" ), digits=2, title="ACS Earnings Major Summary Statistics")
stargazer(subset1w, type="text", summary.stat=c("n", "mean", "median", "sd", "min", "max" ), digits=2, title="ACS Earnings Major Summary Statistics")

# This command finds mean earnings are 187,873.9 and median are 139,437.5,
# This is somewhat higher than Winters (2016) who reported 182,359 and 130,723, respectively.
# Try weighting the average using a regression approach

reg1=lm(INCEARNadj2~econ, data=subset2w, weights=subset2w$PERWT)
summary(reg1)

# Adding the coefficients 149708.6+32649.9=182,358.5. This is identical to Winters.
# And changing econ to history in the code above, 150776.7+10904.8=161,681.5. This is also identical to Winters.
# The dummy variable regression approach works for means but does not produced medians.

# create function that takes a degree/major and calculates frequency 

# create function that takes degree and calculates weighted.mean

# loop function through list of degfield codes 

# loop through top 20 major array and all calc major stats 

calc_major_stats <- function(degfield) {
  # Weighted Arithmetic Mean & Median
  # how do we rename variable name to the degree field
  # should we create a dictionary variable from the code book 
  # create summary stats object overall??? (variance, mean, etc)
  # create data frame by looping through each degfieldd and creating extra data frame with summary stats
  # mean, median, variance, etc 
  
  # change subsets
  # return more than just mean/median
  maj_mean <- weighted.mean(subset(subset2w, DEGFIELDD==degfield)$INCEARNadj2, subset(subset2w,DEGFIELDD==degfield)$PERWT, na.rm = FALSE)
  maj_med <- weighted.median(subset(subset2w, DEGFIELDD==degfield)$INCEARNadj2, subset(subset2w, DEGFIELDD==degfield)$PERWT, na.rm = FALSE)
  
  # change to return dataframe 
  return(maj_mean)
}

# call loop on top 20 majors 
calc_major_stats(5506)

mean_hist=weighted.mean(subset(subset2w, history==1)$INCEARNadj2, subset(subset2w, history==1)$PERWT, na.rm = FALSE)
med_hist=weighted.median(subset(subset2w, history==1)$INCEARNadj2, subset(subset2w, history==1)$PERWT, na.rm = FALSE)

#5506	Political Science and Government

mean_pols=weighted.mean(subset(subset2w, DEGFIELDD==5506)$INCEARNadj2, subset(subset2w, DEGFIELDD==5506)$PERWT, na.rm = FALSE)
med_pols=weighted.median(subset(subset2w, DEGFIELDD==5506)$INCEARNadj2, subset(subset2w, DEGFIELDD==5506)$PERWT, na.rm = FALSE)

#3301	English Language and Literature
#5200	Psychology
#6203	Business Management and Administration
#6201	Accounting
#6200	General Business
#4801	Philosophy and Religious Studies
#6207	Finance
#5301	Criminal Justice and Fire Protection
#5507	Sociology
#1901	Communications
#3600	Biology
#1902	Journalism
#3401	Liberal Arts
#2602	French, German, Latin and Other Common Foreign Language Studies
#5505	International Relations
#1501	Area, Ethnic, and Civilization Studies
#3202	Pre-Law and Legal Studies
#6206	Marketing and Marketing Research
#2408	Electrical Engineering
#5003	Chemistry
#3700	Mathematics
#5502	Anthropology and Archeology
#6402	History - see above, combined major codes
#5501	Economics - see above, combined major codes. remember to merge with other
# related economics majors, such as business econ 


# Overall I exactly replicated 9/25: econ, history, pols, gbus, phil, bio, comm, psyc, prelaw
# The rest were all very close (within < 1%) but were not exact
# all weighted means were exactly identical




