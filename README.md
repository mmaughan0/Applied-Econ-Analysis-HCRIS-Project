########################################################
#Exploratory Analysis of Payer Estimatipon Mehthods
#########################################################



##########################################################
DATA Downloads
#########################################################


#1 Go to the following website to download files - they are not on REPO!
https://www.cms.gov/data-research/statistics-trends-and-reports/cost-reports/hospital-2552-2010-form

#2 Scroll down to :Cost Reports by Fiscal Year" on bottom left

#3 On second page you will find "hospital-2010" link

#4 This will automatically send zip file to your downloads. Open up file:

HOSP10_2022_rpt – this is your master primary key is the cost report number, NOT the hospital provider number
HOSP10_2022_alpha – all alphabetical variables tied to that cost report
HOSP10_2022_nmrc – all numeric variables tied to that cost report

#5 Put these in the source folder of the repo

###########################################################################
#FOLDER STRUCTURE
##########################################################################

/Source - these are unprocessed by any code in this repo. Both the data downloads above and the data I provide in my repo are here
/Intermediate - This is after processing but before outputs. These you will need to produce yourself from the code - as they are also too big to include 
/Code 
00_setup_file - this establishes directories
01_HCRIS_inspect - ensures data uploaded with correct fields, formats etc
02_import_cleanrpt - this integrates an inner join with only hospitals from my list and from the raw files I downloaded. ALso does basic data cleaning
03_working_rpt - this is the workhorse code file - all analysis and outputs done here


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