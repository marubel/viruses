#1 #############################################################################
# Load libraries
library(readxl)
library(tidyverse)
library(plyr)
library(dplyr)
library(lubridate)
library(data.table)
library(tableone)
library(ggplot2)
library(ggpubr)

theme_set(theme_pubr())

#epiDisplay loads MASS which overwrites select, use command to force back to dplyr
select <- dplyr::select

#alternatively could use the below:
library(conflicted)
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("mutate", "dplyr")
conflict_prefer("summarise", "dplyr")
conflict_prefer("rename", "dplyr")
conflict_prefer("arrange", "dplyr")
conflict_prefer("count", "dplyr")
conflict_prefer("first", "dplyr")
conflict_prefer("summarize", "dplyr")
conflict_prefer("between", "dplyr")

#2 #############################################################################
# Load data sources

# Load Dr. Daniel's data
danielsMAR <- read.csv("Z:/users/abe_neuro/COVID19_MAR/Daniels_MAR.csv", fileEncoding = 'UTF-8-BOM', na.strings = c('#NULL!', ''))
# Load image description data (Arterys Listing tab)
imgData <- read.csv("Z:/users/abe_neuro/covid/covid_20201026_abe.csv", fileEncoding = 'UTF-8-BOM', na.strings = c('?', ''))
# Load image severity data
mRaleData <- read.csv("Z:/users/abe_neuro/COVID19_MAR/avemRALE_MRN_11112020.csv", fileEncoding = 'UTF-8-BOM', na.strings = c('NA', ''))

#3 #############################################################################
# Clean Dr. Daniel's DatA

# Create a version of ID with leading 0s in character format
danielsMAR$id_str <- sprintf("%08d", danielsMAR$id)
danielsMAR <- relocate(danielsMAR, id_str, .after = id)

# Convert numeric dates to a date class/format
dateVars <- c("dob", "coviddatefirst", "covidpdate", "doedb", "doeda", "doab", "doaa", "icudate", "intubatedbdate", "intubatedadate", "indexstartdate", "indexenddate", "dod")
danielsMAR <- danielsMAR %>% mutate_at(dateVars, dmy)

# Change "sex" which is 1=male or 2=female, to "female" which is 0=no or 1=yes
danielsMAR <- danielsMAR %>% mutate(female=recode(sex, `1`=0, `2`=1)) %>% relocate(female, .after = sex)

# Added ethnicity/race variables for grant reporting purposes
danielsMAR <- danielsMAR %>% mutate(ethnicity_PHS=recode(ethnicity, 
                                                         'African American'='Non-Hispanic', 
                                                         'Asian/Pacific Islander'='Non-Hispanic',
                                                         'Caucasian'='Non-Hispanic',
                                                         'Hispanic'='Hispanic',
                                                         'Hispanic/Latino'='Hispanic',
                                                         'Multi-Racial'='Non-Hispanic',
                                                         'Non-Hispanic'='Non-Hispanic',
                                                         'Unknown (Patient cannot or refuses to declare ethnicity)'='Unknown',
                                                         .missing='Unknown'
)) %>% relocate(ethnicity_PHS, .after = ethnicity_MAR)
danielsMAR <- danielsMAR %>% mutate(race_PHS=recode(race, 
                                                    'American Indian or Alaska Native'='American Indian/Alaska Native', 
                                                    'Asian'='Asian',
                                                    'Black or African American'='Black or African American',
                                                    'Native Hawaiian or Other Pacific Islander'='Native Hawaiian or Other Pacific Islander',
                                                    'Other Race or Mixed Race'='Unknown or Not Reported',
                                                    'Unknown (Patient cannot or refuses to declare race)'='Unknown or Not Reported',
                                                    'White'='White',
                                                    .missing='Unknown or Not Reported'
)) %>% relocate(race_PHS, .after = ethnicity_PHS)
danielsMAR <- danielsMAR %>% mutate(race_PHS = case_when(
  (ethnicity=='Multi-Racial') ~ 'More than One Race',
  (race_PHS=='Unknown or Not Reported' & ethnicity_MAR=='Asian') ~ 'Asian',
  (race_PHS=='Unknown or Not Reported' & ethnicity_MAR=='Black') ~ 'Black or African American',
  (race_PHS=='Unknown or Not Reported' & ethnicity_MAR=='Middle_Eastern') ~ 'White',
  (race_PHS=='Unknown or Not Reported' & race2=='European') ~ 'White',
  (race_PHS=='Unknown or Not Reported' & race2=='German') ~ 'White',
  (race_PHS=='Unknown or Not Reported' & race2=='French') ~ 'White',
  (race_PHS=='Unknown or Not Reported' & race2=='English') ~ 'White',
  (race_PHS=='Unknown or Not Reported' & race2=='Melanesian') ~ 'Native Hawaiian or Other Pacific Islander',
  (race_PHS=='Unknown or Not Reported' & race2=='Papua New Guinean') ~ 'Native Hawaiian or Other Pacific Islander',
  (race_PHS=='Unknown or Not Reported' & race2=='Other Pacific Islander') ~ 'Native Hawaiian or Other Pacific Islander',
  TRUE ~ race_PHS
))

#4 #############################################################################
# Prep and merge Image Data

# imgData corresponds to the Arterys Listing tab from 10-26-2020 with some modifications
# - Propagate accession number into all rows (was missing from some duplicate phonetic rows)
# - Created a modality column via Excel

# Clean arterys listing data
imgData2 <- imgData %>% 
  select(id_str, accn, phonetic_id, unique_phonetic, study_desc, studyDate, modality, covid_any, available) %>% # get rid of extra columns
  arrange(id_str, phonetic_id, is.na(covid_any), is.na(available)) %>%  # sort so more complete row on top
  distinct(id_str, phonetic_id, .keep_all = TRUE) %>% # keep one
  mutate_at(c("studyDate", "covid_any"), mdy) %>% # convert these to date format
  arrange(id_str, studyDate, phonetic_id) %>% # sort by mrn, study date, and phonetic id
  select(-available) # get rid of 'available' since don't need it after sorting

# Reduce columns in mRALE data $1513x2, remove duplicate rows
mRaleData2 <- mRaleData %>% distinct(unique_phonetic, mean_mRALE, .keep_all = FALSE)

# Merge in severity data to image data
img_mRale_merge = left_join(imgData2, mRaleData2, by='unique_phonetic')

# Check for duplications (phonetic_id is a codified accession number specific to study, but while accession is unique, phonetic_id is not entirely)
# unique_phonetic is a concatenation of MRN and phonetic ID, seems to do a better job thus far at being unique (although technically may not be)
imgData2 %>% count(unique_phonetic, sort=TRUE) %>% filter(n>1) #check unique_phonetic is truly unique in Arterys-listing data
mRaleData2 %>% count(unique_phonetic, sort=TRUE) %>% filter(n>1)  #check unique_phonetic is truly unique in mRale data
img_mRale_merge %>% count(unique_phonetic, sort=TRUE) %>% filter(n>1) #check no duplicate unique_phonetic after merge
dupmerge <- img_mRale_merge %>% count(phonetic_id, sort=TRUE) %>% filter(n>1) 
img_mRale_merge %>% filter(phonetic_id %in% dupmerge$phonetic_id) %>% arrange(phonetic_id) #shows duplicate phonetic IDs
# although there are no duplicate unique_phonetic values after merge
# there are duplicate phonetic_id values, but we are not attaching the mRALE score incorrectly at least by using unique_phonetic

# merge image/severity data to daniel's data
daniels_imgmRale_merge <- left_join(startData, img_mRale_merge, by='id_str')
daniels_imgmRale_merge$study_indexstart_deltaT <- difftime(daniels_imgmRale_merge$studyDate, daniels_imgmRale_merge$indexstartdate, units = 'days')
daniels_imgmRale_merge$study_indexend_deltaT <- difftime(daniels_imgmRale_merge$studyDate, daniels_imgmRale_merge$indexenddate, units = 'days')
daniels_imgmRale_merge$study_icudate_deltaT <- difftime(daniels_imgmRale_merge$studyDate, daniels_imgmRale_merge$icudate, units = 'days')
daniels_imgmRale_merge <- daniels_imgmRale_merge %>% arrange(id_str, study_indexstart_deltaT)
# temporarily reduce # of columns for readability
tempMerge1 <- daniels_imgmRale_merge %>% select(id_str, unique_phonetic, accn, phonetic_id, study_desc, modality, studyDate, indexstartdate, indexenddate, icudate, study_indexstart_deltaT, study_icudate_deltaT, study_indexend_deltaT, mean_mRALE, ethnicity_PHS, race_PHS, female)

# Capturing exclusion #s
tempMerge2 <- tempMerge1 %>% group_by(id_str)
inclusion_list <- c(as.numeric(length(unique(tempMerge2$id_str))))
# Filter no imaging study associated with MRN
tempMerge2 <- tempMerge2 %>% filter(!is.na(phonetic_id))
inclusion_list <- c(inclusion_list, as.numeric(length(unique(tempMerge2$id_str))))
# dropping studies that are more than 30 days before indexstart
tempMerge2 <- tempMerge2 %>% filter(as.numeric(study_indexstart_deltaT)>-30)
inclusion_list <- c(inclusion_list, as.numeric(length(unique(tempMerge2$id_str))))
#dropping studies that are more than 30 days after indexstart
tempMerge2 <- tempMerge2 %>% filter(as.numeric(study_indexstart_deltaT)<30)
inclusion_list <- c(inclusion_list, as.numeric(length(unique(tempMerge2$id_str))))
#Number of patient's with XR or CT
tempMerge2 <- tempMerge2 %>% filter(modality=="XR" | modality=="CT")
inclusion_list <- c(inclusion_list, as.numeric(length(unique(tempMerge2$id_str))))
print(inclusion_list)
exclusion_label <- c("No imaging study \nassociated with MRN", 
                     "Only studies more than \n30 days before index start date", 
                     "Only studies less than \n30 days after index start date",
                     "Patients without XR or CT")
inclusion_label <- c("Has at least 1 study \nin Arterys-Listing \nassociated with MRN",
                     "Has studies after 30 days \nbefore index start date", 
                     "Has studies before 30 days \nafter index start date",
                     "Remaining after filtering for XR or CT")
flowgraph2 <- PRISMAstatement::flow_exclusions(
  incl_counts = inclusion_list,
  total_label = "Total unique patients",
  incl_labels = inclusion_label,
  excl_labels = exclusion_label,
  height = 4800,
  width = 4800)
flowgraph2

tempMerge3 <- tempMerge2 %>%
  arrange(id_str, abs(study_indexstart_deltaT), mean_mRALE) %>%
  group_by(id_str) %>%
  slice(1)

tempMerge_notHispanic <- tempMerge3 %>% filter(ethnicity_PHS=="Non-Hispanic")
tempMerge_Hispanic <- tempMerge3 %>% filter(ethnicity_PHS=="Hispanic")
tempMerge_Unknown <- tempMerge3 %>% filter(ethnicity_PHS=="Unknown")

racetab_notHispanic <- CreateTableOne(vars = c('race_PHS'), factor = c('race_PHS'), strata = c("female"), data = tempMerge_notHispanic, includeNA = T)
racetab_Hispanic <- CreateTableOne(vars = c('race_PHS'), factor = c('race_PHS'), strata = c("female"), data = tempMerge_Hispanic, includeNA = T)
racetab_Unknown <- CreateTableOne(vars = c('race_PHS'), factor = c('race_PHS'), strata = c("female"), data = tempMerge_Unknown, includeNA = T)

racetabcsv_notHispanic <- print(racetab_notHispanic, showAllLevels = TRUE, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
racetabcsv_Hispanic <- print(racetab_Hispanic, showAllLevels = TRUE, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
racetabcsv_Unknown <- print(racetab_Unknown, showAllLevels = TRUE, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)

write.csv(racetabcsv_notHispanic, file = "Z:\\users\\abe_neuro\\RProjects\\RaceEthnicityGenderTable_notHispanic.csv")
write.csv(racetabcsv_Hispanic, file = "Z:\\users\\abe_neuro\\RProjects\\RaceEthnicityGenderTable_Hispanic.csv")
write.csv(racetabcsv_Unknown, file = "Z:\\users\\abe_neuro\\RProjects\\RaceEthnicityGenderTable_Unknown.csv")