#==============================================================================
#   Analysis file for Costa and Kahn reproduction
#==============================================================================

# This file reproduces the results from Costa and Kahn (2011). This version is by Matt Holian 7/5/2019.

# Citation: Costa, Dora L., and Matthew E. Kahn. "Electricity consumption and durable housing: understanding cohort effects." 
# American Economic Review 101, no. 3 (2011): 88-92.

# Download the data, unzip it and save the files Table1data.dta and price57.dta to your directory folder:
# https://www.aeaweb.org/aer/data/may2011/P2011_1614_data.zip

# The published version of the Costa and Kahn article:
# https://www.aeaweb.org/articles?id=10.1257/aer.101.3.88

# The working paper version of this article contains the full regression results in Table 5:
# https://www.nber.org/papers/w15978.pdf


#==============================================================================
#   1. Settings, packages, and options
#==============================================================================

# Clear the working space
rm(list = ls())

# Set working directory 
setwd("/Users/mattholian/Desktop/StoriesAndStatistics/Rscripts/CKreproduction")

# Load the packages; 
# stargazer is for creating tables of summary statistics and regression results
# car is for recoding; plyr contains the function "count"
# lfe is for clustered standard errors; foreign is for opening Stata data files

library(stargazer)
library(car)
library(plyr)
library(lfe)
library(foreign)

# turn off scientific notation except for big numbers
options(scipen = 9)


#==============================================================================
#   2. Data section
#==============================================================================

# Read data from dta file

census <- read.dta("Table1data.dta", convert.factors=FALSE, convert.dates=TRUE, convert.underscore=TRUE, warn.missing.labels=TRUE)
district <- read.dta("price57.dta")

# The Table1data.dta contains data from the 2000 5% Census with restrictions for: householders, in CA, single-family homes (unitsstr 3 and 4), owners, aged 30-65
# Further data restrictions added here: year 2000, built between 1960-2000, non-negative income

census2 = subset(census, year==2000 & builtyr<7 & hhincome>0)

# rename NUMPREC 

census2$hhsize = census2$numprec

# generate log income

census2$lhhincome = log(census2$hhincome)

# generate log electricity expenditure

census2$costelec = recode(census2$costelec, "9993:9999=NA")
census2$lcostelec = log(census2$costelec)

# basic tabulate command
x=count(census2, 'builtyr')
x
# 01 1999 or later
# 02 1995-1998
# 03 1990-1994
# 04 1980-1989
# 05 1970-1979
# 06 1960-1969

census2$YB1960=recode(census2$builtyr, "6=1; else=0") 
census2$YB1970=recode(census2$builtyr, "5=1; else=0") 
census2$YB1980=recode(census2$builtyr, "4=1; else=0") 
census2$YB1990_94=recode(census2$builtyr, "3=1; else=0") 
census2$YB1995_98=recode(census2$builtyr, "2=1; else=0") 
census2$YB1999_00=recode(census2$builtyr, "1=1; else=0") 

# generate white dummy 

census2$white=recode(census2$raced, "100=1; else=0")

# rescale SEI  

census2$sei=census2$sei/100

# generate electric heat dummy (4=1, else =0)

census2$eleheat=recode(census2$fuelheat, "4=1; else=0")

# merge the price data to the census2 data; begin by creating common identifying variable

census2$pumabuiltyr = (census2$puma)*10 + (census2$builtyr)
district$pumabuiltyr = (district$puma)*10 + (district$cat)

common = merge(census2, district, by="pumabuiltyr")
common$puma = common$puma.x
common$lprice = log(common$price)


#==============================================================================
#   3. Analysis section
#==============================================================================

# create a table of summary statistics

stargazer(data=common, type="text", summary.stat=c("n", "mean", "median", "sd", "min", "max" ), digits=2, title="2000 5% Census CA Summary Statistics")

# run regressions 

reg1 = felm(lcostelec~age+rooms+lhhincome+hhsize+white+eleheat+sei+YB1960+YB1970+YB1980+YB1990_94+YB1995_98 | puma | 0 | price, data=common, weights=common$hhwt)
reg2 = felm(lcostelec~lprice+age+rooms+lhhincome+hhsize+white+eleheat+sei+YB1960+YB1970+YB1980+YB1990_94+YB1995_98 | puma | 0 | price, data=common, weights=common$hhwt)

# create a table of regression results

stargazer(reg1,	reg2,
          title="Costa Kahn Reproduction, Regression Results", type="text", 
          df=FALSE, digits=3)

