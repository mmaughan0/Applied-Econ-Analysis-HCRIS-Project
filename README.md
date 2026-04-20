########################################################
#Exploratory Analysis of Payer Estimatipon Mehthods
#########################################################



##########################################################
DATA Downloads
#########################################################


#Go to the following website to download files - they are not on REPO!
https://www.cms.gov/data-research/statistics-trends-and-reports/cost-reports/hospital-2552-2010-form
Scroll down to "Downloads" and click on Hospital 2010 Documentation

You will find many of the files on this repo in there - only 3 you will not find that you need to input

HOSP10_2022_rpt – this is your master primary key is the cost report number, NOT the hospital provider number
HOSP10_2022_alpha – all alphabetical variables tied to that cost report
HOSP10_2022_nmrc – all numeric variables tied to that cost report

#######################################
DATA from REPO
######################################

There are in addition 2 files that I have provided in the repo you will need that are relatively small 
-analytical_sample.csv - this corresponds to a list of hospitals after applying inclusion/exclusion criteria - It greatly cuts down on size and helps workflow, hence it is early in my analytical workflow

-State_Price_Estimation_ExAnte.csv - This is the motivating reason for this analysis - to compare raw data that we are procesing to work already done

The gitignore file ignores the rest of source, BUT it brings in these. These were already synthesized by me and you cannot find them publicly

#################################################################
OUTPUTS FOR REPRODUCIBILITY
################################################################

I am asking to reproduce the 4 scatterplots found in the output file - might be helpful to append your initials so you can compare yours to the original