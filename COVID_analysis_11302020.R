# Changelog
# 11/30/2020 
#   - Cleaned 11/5 version, begin tracking changes


###############################################################################
# -----------------                                                           #
#|Table of Contents|           (Search #1, #2, etc to skip to section)        #
# -----------------                                                           #
#1 Load libraries                                                             #
#2 Load data sources                                                          #
#3 Clean Dr. Daniel's Data                                                    #
#4 Prep and merge Image Data                                                  #
#5 Prep run final PCAs w/ img Vars                                            #
#6 Data tabulation/TableOnes                                                  #
#7 Survival analysis                                                          #
###############################################################################

#1 #############################################################################
# Load libraries
library(readxl)
library(tidyverse)
library(dplyr)
library(lubridate)
library(data.table)
library(factoextra)
library(corrplot)
library(FactoMineR)
library(factoextra)
library(FactoInvestigate)
library(tableone)
library(survival)
library(survminer)
library(epiDisplay)

#epiDisplay loads MASS which overwrites select, use command to force back to dplyr
select <- dplyr::select

#alternatively could use the below:
library(conflicted)
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")


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

# General categorization of variables into list (will help construct subsets later)
basicVars <- c("id", "id_str", "coviddatefirst", "transfer", "transferother")
demogVars <- c("female", "dob", "covidage", "covidage10", "hcwo", "ethnicity_MAR", "insurance")
comorbVars <- c("Asthma_PT", "COPD_PT", "CKD_PT", "Diabetes_PT", "Prediabetes_PT", "Hypertension_PT", "Tobacco_PT", "HIV_PT", "Obesity_PT", "CirrLiv_PT", "IschVasc_PT", "Stroke_PT", "CF_PT", "HFailure_PT", "CVD_PT")
comorbVars2 <- c("tobfreq", "tobyrs", "etoh", "dm", "dmtype", "dmmed", "dminsulin", "dmoral", "osa", "copd", "asthma", "cad", "chf", "af", "stroke", "tia", "pad", "cvsurg", "cabg", "cabgyr", "pci", "pciyr", "valve", "pm", "icd", "abltn", "tx", "txyr", "strokesurg", "pvdsurg", "pvdsurgyr", "ckd", "cancer", "cancertype", "cirrhosis", "hiv", "immunodx", "transplantany", "transorgan", "chemo", "imab", "infect", "inflam", "inflamtype", "neuro", "gi") 
comorbVars3 <- c("asthma2", "copd2", "ckd2", "diabetes2", "prediabetes2", "htn2", "tob2", "hiv2", "obesity2", "CirrLiv2", "IschVasc2", "stroke2", "CF2", "HF2", "CVD2", "obesity2b", "tobcurr")
vitalVars <- c("bp", "pulse", "osat", "temp", "fever", "pregnant", "sbp1st", "sbphigh", "sbplow", "dbp1st", "dbphigh", "dbplow", "pulse1st", "pulsehigh", "pulselow", "osat1st", "osathigh", "osatlow", "tempadmit")
edVars <- c("edb", "doedb", "eda", "doeda")
admitVars <- c("admitb", "doab", "admita", "doaa", "los")
icuVars <- c("icu", "icudate", "losicu")
icuRxVars <- c("ventb", "venta", "intubatedb", "intubatedbdate", "intubateda", "intubatedadate", "ecmob", "ecmoa", "crrtb", "crrta")
miscVars <- c("indexstartdate", "indexenddate")
dispoVars <- c("dispo", "death", "dod")
medicationVars <- c("acei", "aceiprior", "arb", "arbprior", "asa", "asaprior", "ibuprof", "ibuprofprior", "nsaid", "nsaidprior", "med", "medhtn", "ccb", "bb", "diuretic", "aldo", "bpother", "medchol", "medstatin", "bldthin", "chloro", "chloroinpt", "hydroxy", "hydroxyinpt", "azithro", "azithroinpt", "remdesivir", "solumedrol", "prednisone", "predpast", "sirolimus", "sirolimuspast")
medicationdoseVars <- c("aceidose", "aceipriordose", "arbdose", "arbpriordose", "asadose", "asapriordose", "ibuprofdose", "ibuprofpriordose", "nsaiddose", "nsaidpriordose")
medicationVars2 <- c("aceieverp", "arbeverp", "asaeverp", "ibuprofeverp", "nsaideverp", "acearb", "bpmedother")
labVars <- c("bnp", "bnpp", "hstrop", "hstropref", "procalc", "ddimer", "hscrp", "crp", "gfr", "creat", "bun", "sodium", "pot", "alb", "ast", "alt", "ldh", "ferritin", "wbc", "rdw", "lymph", "anc", "hgb", "hct", "plt", "inr", "ptt", "flua", "flub")
labVars2 <- c("lgBNPP", "lgtrop", "trop99s", "trop99", "BNPPyn", "BNPyn", "BNPany")
echoecgVars <- c("echodate", "lavi", "rvddbasal", "rvddmid", "lvidd", "lvids", "ivcd", "trvel", "pap", "eeprimeratio", "lvef", "ecgdate", "vr", "ar", "qrs", "qtc", "qt", "raxis", "taxis")
miscfinalVars <- c("echo2", "vent", "intubated", "ecmo", "crrt", "admit", "ED", "admitcovid", "valid", "cvdLD", "icu2", "severe", "severei", "admitvalid", "severecovid", "severevalid", "cadLD", "cvdreg", "cvdhfLD", "cvdhfReg", "cvdafLD", "cvdafreg")

# Check number of NAs for each variable in original Daniel's data
na_count <-sapply(danielsMAR, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
# Output to csv (modify path as needed)
write.csv(na_count,"Z:\\users\\abe_neuro\\RProjects\\Daniels_NA_Count.csv", row.names = TRUE)

# Check number of variables with all 0s for values
zero_count <- danielsMAR %>% summarise(across(where(is.numeric), sum,na.rm=TRUE))
zero_count <- data.frame(as.data.frame(t(as.matrix(zero_count))))
zero_count <- zero_count %>% filter(V1==0) %>% rename("zero_count"="V1")
write.csv(zero_count,"Z:\\users\\abe_neuro\\RProjects\\Daniels_Zero_Count.csv", row.names = TRUE)

# Slim our data down to include only some of the groups above (get rid of some columns with large amounts of missing data)
# For example, the "sp" variables are drug names, but very rarely have this
# Dose amounts are in text format, some times mg, some times tablet, variation within same class of drug, too granular and too few non-NA values for our purposes
# Date that certain diagnoses added not very useful and data is sparse
startData <- danielsMAR %>% select(
  all_of(basicVars), all_of(demogVars), all_of(comorbVars3), all_of(vitalVars), all_of(medicationVars), 
  all_of(labVars), all_of(echoecgVars), all_of(miscfinalVars), all_of(edVars), all_of(admitVars), 
  all_of(icuVars), all_of(dispoVars), all_of(miscVars)
  )


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
tempMerge1 <- daniels_imgmRale_merge %>% select(id_str, unique_phonetic, accn, phonetic_id, study_desc, modality, studyDate, indexstartdate, indexenddate, icudate, study_indexstart_deltaT, study_icudate_deltaT, study_indexend_deltaT, mean_mRALE)

# Capturing exclusion #s
tempMerge2 <- tempMerge1 %>% group_by(id_str)
inclusion_list <- c(as.numeric(length(unique(tempMerge2$id_str))))
# Filter no imaging study associated with MRN
tempMerge2 <- tempMerge2 %>% filter(!is.na(phonetic_id))
inclusion_list <- c(inclusion_list, as.numeric(length(unique(tempMerge2$id_str))))
# dropping studies that are more than 7 days away from indexstart, 599 -> 533
tempMerge2 <- tempMerge2 %>% filter(as.numeric(study_indexstart_deltaT)>-7)
inclusion_list <- c(inclusion_list, as.numeric(length(unique(tempMerge2$id_str))))
#dropping studies that are after date of ICU admission 533 -> 480
tempMerge2 <- tempMerge2 %>% filter(as.numeric(study_icudate_deltaT)<0 | is.na(icudate)) 
inclusion_list <- c(inclusion_list, as.numeric(length(unique(tempMerge2$id_str))))
#Number of patient's with an X-ray after exclusions = 448
tempMerge2 <- tempMerge2 %>% filter(modality=="XR")
inclusion_list <- c(inclusion_list, as.numeric(length(unique(tempMerge2$id_str))))
print(inclusion_list)

tempMerge3 <- tempMerge1 %>% filter(!is.na(phonetic_id)) %>% filter(modality=="XR") %>% group_by(id_str)
length(unique(tempMerge3$id_str)) #Total Number of patient's with an X-ray = 561 (before exclusions)

#Generate flow chart of exclusions
#inclusion_list <- c(826, 599, 533, 480, 448)
exclusion_label <- c("No imaging study \nassociated with MRN", 
                     "Only studies more than \n7 days before index start date", 
                     "Only studies after \nICU admission",
                     "Patients without Xrays (CT only)")
inclusion_label <- c("Has at least 1 study \nin Arterys-Listing \nassociated with MRN",
                     "Has studies after 7 days \nbefore index start date", 
                     "Has studies before ICU admission \nor never admitted to ICU",
                     "Remaining after filtering for XR only")

flowgraph2 <- PRISMAstatement::flow_exclusions(
  incl_counts = inclusion_list,
  total_label = "Total unique patients",
  incl_labels = inclusion_label,
  excl_labels = exclusion_label,
  height = 4800,
  width = 4800)

flowgraph2

#Generate graph of distribution of number of CXRs per patient
XRstudies_count <- tempMerge2 %>% count(id_str)
summary(XRstudies_count$n)
hist(XRstudies_count$n, breaks=c(1:max(XRstudies_count$n)), main="Distribution of # of CXRs per patient", xlab="No. of CXRs", ylab="No. of Patients", xlim=c(0,55))
axis(1, at=seq(0, 55, by=5))

# Bring in mRale variables (1st, low, high) with respective studyDate and phonetic_id
mralevars_merge <- tempMerge1 %>% 
  filter(studyDate>=indexstartdate-1, studyDate<=indexenddate, modality!='CT', grepl("SPINE", study_desc)==FALSE) %>%
  arrange(id_str, studyDate) %>%
  group_by(id_str) %>%
  mutate(mrale1st = first(mean_mRALE), pid_mrale1st = first(phonetic_id), studyD_mrale1st = first(studyDate)) %>% # 1st mRale with associated phonetic/studyDate
  ungroup(id_str)
mralevars_merge <- mralevars_merge %>%
  filter(studyDate>=indexstartdate-1, studyDate<=indexenddate, modality!='CT', grepl("SPINE", study_desc)==FALSE) %>%
  arrange(id_str, desc(mean_mRALE), studyDate) %>%
  group_by(id_str) %>%
  slice(1) %>%
  mutate(mralehigh = mean_mRALE, pid_mralehigh = phonetic_id, studyD_mralehigh = studyDate) %>%
  select(id_str, mralehigh, pid_mralehigh, studyD_mralehigh) %>%
  left_join(mralevars_merge, by='id_str') %>%
  relocate(c(mralehigh, pid_mralehigh, studyD_mralehigh), .after = studyD_mrale1st)
mralevars_merge <- mralevars_merge %>%
  filter(studyDate>=indexstartdate-1, studyDate<=indexenddate, modality!='CT', grepl("SPINE", study_desc)==FALSE) %>%
  arrange(id_str, mean_mRALE, studyDate) %>%
  group_by(id_str) %>%
  slice(1) %>%
  mutate(mralelow = mean_mRALE, pid_mralelow = phonetic_id, studyD_mralelow = studyDate) %>%
  select(id_str, mralelow, pid_mralelow, studyD_mralelow) %>%
  left_join(mralevars_merge, by='id_str') %>%
  relocate(c(mralelow, pid_mralelow, studyD_mralelow), .after = studyD_mralehigh)

# Define a severity over time function
severity_over_time <- function(sev_high, sev_start, date_high, date_start){
  delta_sev <- sev_high - sev_start
  delta_time <- as.numeric(difftime(date_high, date_start, units = 'days'))
  sev_over_time <- delta_sev / delta_time
  if(is.na(sev_high) | is.na(sev_start)){
    sev_over_time <- NA
  }else if(as.numeric(date_high) == as.numeric(date_start) | as.numeric(date_high) < as.numeric(date_start)){
    sev_over_time <- 0
  }
  return(sev_over_time)
}

mraleVars <- c("mrale1st", "pid_mrale1st", "studyD_mrale1st", "mralehigh", "pid_mralehigh", "studyD_mralehigh", "mralelow", "pid_mralelow", "studyD_mralelow", "sev_over_time")
mralevars_merge <- mralevars_merge %>%
  mutate(sev_over_time = severity_over_time(mralehigh, mrale1st, studyD_mralehigh, studyD_mrale1st)) %>%
  slice(1) %>%
  select("id_str", mraleVars) %>%
  ungroup("id_str")

# Remove some temporary objects no longer needed
rm(tempMerge1, tempMerge2, tempMerge3)

# Want to merge in our mRale variables into our start Data so we are back to 1 row per MRN
finalMerge <- left_join(startData, mralevars_merge, by="id_str")
write.csv(finalMerge,"Z:\\users\\abe_neuro\\RProjects\\DanielsData_mRaleMerged_20201111.csv", row.names = TRUE)

#5 #############################################################################
# Prep Final PCAs

# Now we need to define our individual populations
everyoneDF <- finalMerge
ethnicity_map <- c("White" = 1, "Black" = 2, "Hispanic" = 3, "Asian" = 4, "Middle_Eastern" = 5, "Other_Background" = 6, "Unknown" = 9)
everyoneDF$ethnicity_MAR <- ethnicity_map[everyoneDF$ethnicity_MAR]
# Make insurance numeric categories (made my own number scheme)
insurance_map <- c("NA" = 9, "Commerical" = 1, "Medicare" = 2, "Medicare Managed Care" = 3, "Medicaid - California" = 4, "Medicaid - Out of State" = 4, "Medicaid Managed Care" = 5, "County Medical Services" = 6, "Workers Compensation" = 7, "Other Government" = 8)
everyoneDF$insurance <- insurance_map[everyoneDF$insurance]
# Set missing insurance data to 9 (Unknown)
everyoneDF$insurance[is.na(everyoneDF$insurance)] <- 9
# Make the date numeric so PCA commands can run on them
everyoneDF$coviddatefirst <- as.numeric(everyoneDF$coviddatefirst)
# Make id_str the index or row name
rownames(everyoneDF) <- everyoneDF$id_str
everyoneDF <- select(everyoneDF, -id_str, -id,
                     -bp, -pulse, -osat, -temp, -fever, #removing these vital stats, as mostly captured in vital1st, overly redundant
                     -dob, -covidage10, #redundant with covidage
                     -flua, -flub, #too many missing
                     -severe, -severei, -severecovid, -severevalid, #Not sure how these are decided    
                     -edb, -doedb, -eda, -doeda, -admita, -doaa, -admitb, -doab, -los, -icu, -icudate, -losicu, -dispo, -death, -dod, #outcomes
                     -valid, -admitvalid, # not sure what these are, maybe the people that were individually looked at?
                     -chloro, -chloroinpt, -hydroxyinpt, -predpast, -sirolimuspast, -CF2, #All 0s (no variance)
                     -echodate, -ecgdate, -rvddbasal, -rvddmid, -lvidd, -lvids, -trvel, -eeprimeratio, #All missing
                     #-cvdLD, -cadLD, -cvdreg, -cvdhfLD, -cvdhfReg, -cvdafLD, -cvdafreg, 
                     -indexstartdate, -indexenddate, -pid_mrale1st, -studyD_mrale1st, -pid_mralehigh, -studyD_mralehigh, -pid_mralelow, -studyD_mralelow
                     )

admitDF <- everyoneDF %>%
  filter(admit==1)
admitcovidDF <- everyoneDF %>%
  filter(admitcovid==1)
edonlyDF <- everyoneDF %>%
  filter(ED==1 & admit==0) %>%
  # Need to remove some columns with nearly all missing values to run the PCA
  select(-bnp, -bnpp, -hstrop, -hstropref, -procalc, -ddimer, -hscrp, -crp, -gfr, -ldh, -ferritin, -inr, -ptt)
edDF <- everyoneDF %>%
  filter(ED==1)
icuonlyDF <- everyoneDF %>%
  filter(icu2==1)

everyoneDF <- select(everyoneDF, -admit, -ED, -admitcovid, -icu2)

nrow(everyoneDF)
nrow(admitDF)
nrow(admitcovidDF)
nrow(edonlyDF)
nrow(edDF)
nrow(icuonlyDF)

# How found missing values to remove
#na_count.ed <-sapply(edonlyDF, function(y) sum(length(which(is.na(y)))))
#na_count.ed <- data.frame(na_count.ed)
#na_count.ed

# Run PCAs
everyonePCA <- PCA(everyoneDF, graph = FALSE)
admitPCA <- PCA(admitDF, graph = FALSE)
admitcovidPCA <- PCA(admitcovidDF, graph = FALSE)
edonlyPCA <- PCA(edonlyDF, graph = FALSE)
edPCA <- PCA(edDF, graph = FALSE)
icuonlyPCA <- PCA(icuonlyDF, graph = FALSE)


Investigate(everyonePCA, file="Z:/users/abe_neuro/covid/everyonePCA_Investigate.rmd", document = "pdf_document")
Investigate(admitPCA, file="Z:/users/abe_neuro/covid/admitPCA_Investigate.rmd", document = "pdf_document")
Investigate(edonlyPCA, file="Z:/users/abe_neuro/covid/edonlyPCA_Investigate.rmd", document = "pdf_document")
Investigate(edPCA, file="Z:/users/abe_neuro/covid/edPCA_Investigate.rmd", document = "pdf_document")
Investigate(icuonlyPCA, file="Z:/users/abe_neuro/covid/icuonlyPCA_Investigate.rmd", document = "pdf_document")

#Plots for everyonePCA

fviz_eig(everyonePCA, ncp=15, linecolor='red', main="Everyone") + theme(plot.title = element_text(hjust = 0.5))
plot(everyonePCA$eig[1:(length(everyonePCA$eig)/3),3], ylab = "Cumulative percent of Variance Explained", ylim=c(0,100), xlab = "Principal Component", type = 'b', main="Everyone")
fviz_pca_var(everyonePCA, axes=c(1,2), col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE, select.var = list(contrib = 30), title="Variables - Everyone PCA") + theme(plot.title = element_text(hjust = 0.5))
fviz_pca_var(everyonePCA, axes=c(2,3), col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE, select.var = list(contrib = 30), title="Variables - Everyone PCA") + theme(plot.title = element_text(hjust = 0.5))
fviz_pca_var(everyonePCA, axes=c(3,4), col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE, select.var = list(contrib = 30), title="Variables - Everyone PCA") + theme(plot.title = element_text(hjust = 0.5))
fviz_contrib(everyonePCA, choice = "var", axes = 1, top = 15, title="Everyone: Contribution of vars to PC1")+ theme(plot.title = element_text(hjust = 0.5))
fviz_contrib(everyonePCA, choice = "var", axes = 2, top = 15, title="Everyone: Contribution of vars to PC2")+ theme(plot.title = element_text(hjust = 0.5))
fviz_contrib(everyonePCA, choice = "var", axes = 3, top = 15, title="Everyone: Contribution of vars to PC3")+ theme(plot.title = element_text(hjust = 0.5))
fviz_contrib(everyonePCA, choice = "var", axes = 4, top = 15, title="Everyone: Contribution of vars to PC4")+ theme(plot.title = element_text(hjust = 0.5))
fviz_contrib(everyonePCA, choice = "var", axes = 5, top = 15, title="Everyone: Contribution of vars to PC5")+ theme(plot.title = element_text(hjust = 0.5))

#Plots for admitPCA (Admits only)
fviz_eig(admitPCA, ncp=15, linecolor='red', main="Admitted") + theme(plot.title = element_text(hjust = 0.5)) #Scree Plot
plot(admitPCA$eig[1:(length(admitPCA$eig)/3),3], ylab = "Cumulative percent of Variance Explained", ylim=c(0,100), xlab = "Principal Component", type = 'b', main="Admitted")
fviz_pca_var(admitPCA, axes=c(1,2), col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE, select.var = list(contrib = 30), title="Variables - Admitted PCA") + theme(plot.title = element_text(hjust = 0.5))
fviz_contrib(admitPCA, choice = "var", axes = 1, top = 15, title="Admitted: Contribution of vars to PC1")+ theme(plot.title = element_text(hjust = 0.5))
fviz_contrib(admitPCA, choice = "var", axes = 2, top = 15, title="Admitted: Contribution of vars to PC2")+ theme(plot.title = element_text(hjust = 0.5))
fviz_contrib(admitPCA, choice = "var", axes = 3, top = 15, title="Admitted: Contribution of vars to PC3")+ theme(plot.title = element_text(hjust = 0.5))
fviz_contrib(admitPCA, choice = "var", axes = 4, top = 15, title="Admitted: Contribution of vars to PC4")+ theme(plot.title = element_text(hjust = 0.5))
fviz_contrib(admitPCA, choice = "var", axes = 5, top = 15, title="Admitted: Contribution of vars to PC5")+ theme(plot.title = element_text(hjust = 0.5))

#Plots for edonlyPCA (At ED, but not admitted)
fviz_eig(edonlyPCA, ncp=15, linecolor='red', main="ED Only") + theme(plot.title = element_text(hjust = 0.5)) #Scree Plot
plot(edonlyPCA$eig[1:(length(edonlyPCA$eig)/3),3], ylab = "Cumulative percent of Variance Explained", ylim=c(0,100), xlab = "Principal Component", type = 'b', main="ED Only (not Admit)")
fviz_pca_var(edonlyPCA, axes=c(1,2), col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE, select.var = list(contrib = 30), title="Variables - ED Only PCA") + theme(plot.title = element_text(hjust = 0.5))
fviz_contrib(edonlyPCA, choice = "var", axes = 1, top = 15, title="ED Only: Contribution of vars to PC1")+ theme(plot.title = element_text(hjust = 0.5))
fviz_contrib(edonlyPCA, choice = "var", axes = 2, top = 15, title="ED Only: Contribution of vars to PC2")+ theme(plot.title = element_text(hjust = 0.5))
fviz_contrib(edonlyPCA, choice = "var", axes = 3, top = 15, title="ED Only: Contribution of vars to PC3")+ theme(plot.title = element_text(hjust = 0.5))
fviz_contrib(edonlyPCA, choice = "var", axes = 4, top = 15, title="ED Only: Contribution of vars to PC4")+ theme(plot.title = element_text(hjust = 0.5))
fviz_contrib(edonlyPCA, choice = "var", axes = 5, top = 15, title="ED Only: Contribution of vars to PC5")+ theme(plot.title = element_text(hjust = 0.5))

#Plots for icuonlyPCA (anyone in ICU)
fviz_eig(icuonlyPCA, ncp=15, linecolor='red', main="ICU Only") + theme(plot.title = element_text(hjust = 0.5)) #Scree Plot
plot(icuonlyPCA$eig[1:(length(icuonlyPCA$eig)/3),3], ylab = "Cumulative percent of Variance Explained", ylim=c(0,100), xlab = "Principal Component", type = 'b', main="ICU Only")
fviz_contrib(icuonlyPCA, choice = "var", axes = 1, top = 15, title="ICU Only: Contribution of vars to PC1")+ theme(plot.title = element_text(hjust = 0.5))
fviz_contrib(icuonlyPCA, choice = "var", axes = 2, top = 15, title="ICU Only: Contribution of vars to PC2")+ theme(plot.title = element_text(hjust = 0.5))
fviz_contrib(icuonlyPCA, choice = "var", axes = 3, top = 15, title="ICU Only: Contribution of vars to PC3")+ theme(plot.title = element_text(hjust = 0.5))
fviz_contrib(icuonlyPCA, choice = "var", axes = 4, top = 15, title="ICU Only: Contribution of vars to PC4")+ theme(plot.title = element_text(hjust = 0.5))
fviz_contrib(icuonlyPCA, choice = "var", axes = 5, top = 15, title="ICU Only: Contribution of vars to PC5")+ theme(plot.title = element_text(hjust = 0.5))

#Plots for admitcovidPCA (Admits only)
fviz_eig(admitcovidPCA, ncp=15, linecolor='red', main="Admitted") + theme(plot.title = element_text(hjust = 0.5)) #Scree Plot
plot(admitcovidPCA$eig[1:(length(admitcovidPCA$eig)/3),3], ylab = "Cumulative percent of Variance Explained", ylim=c(0,100), xlab = "Principal Component", type = 'b', main="Admitted")
fviz_pca_var(admitcovidPCA, axes=c(1,2), col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE, select.var = list(contrib = 30), title="Variables - Admitted PCA") + theme(plot.title = element_text(hjust = 0.5))
fviz_contrib(admitcovidPCA, choice = "var", axes = 1, top = 15, title="Admitted: Contribution of vars to PC1")+ theme(plot.title = element_text(hjust = 0.5))
fviz_contrib(admitcovidPCA, choice = "var", axes = 2, top = 15, title="Admitted: Contribution of vars to PC2")+ theme(plot.title = element_text(hjust = 0.5))
fviz_contrib(admitcovidPCA, choice = "var", axes = 3, top = 15, title="Admitted: Contribution of vars to PC3")+ theme(plot.title = element_text(hjust = 0.5))
fviz_contrib(admitcovidPCA, choice = "var", axes = 4, top = 15, title="Admitted: Contribution of vars to PC4")+ theme(plot.title = element_text(hjust = 0.5))
fviz_contrib(admitcovidPCA, choice = "var", axes = 5, top = 15, title="Admitted: Contribution of vars to PC5")+ theme(plot.title = element_text(hjust = 0.5))

summary(everyoneDF$mrale1st)


#6 #############################################################################
# Data tabulation/TableOnes

#Create some tables now that have variables you are interested
IncludedData <- finalMerge
IncludedData <- IncludedData %>% mutate(after_july1 = if_else(coviddatefirst>=mdy("07-01-2020"),1,0))

myVars <- c("covidage", "ethnicity_MAR", "insurance", "pregnant", "after_july1", 
            "asthma2", "copd2", "ckd2", "diabetes2", "prediabetes2", "htn2", "tob2", "hiv2", "obesity2", "CirrLiv2", "IschVasc2", "stroke2", "CF2", "HF2", "CVD2", "obesity2b", "tobcurr",
            "sbp1st", "sbphigh", "sbplow", "dbp1st", "dbphigh", "dbplow", "pulse1st", "pulsehigh", "pulselow", "osat1st", "osathigh", "osatlow", "tempadmit",
            "acei", "aceiprior", "arb", "arbprior", "asa", "asaprior", "ibuprof", "ibuprofprior", "nsaid", "nsaidprior", "med", "medhtn", "ccb", "bb", "diuretic", "aldo", "bpother", "medchol", "medstatin", "bldthin", "remdesivir", "solumedrol", "prednisone",
            "bnp", "bnpp", "procalc", "ddimer", "hscrp", "crp", "gfr", "creat", "bun", "sodium", "pot", "alb", "ast", "alt", "ldh", "ferritin", "wbc", "rdw", "lymph", "anc", "hgb", "hct", "plt", "inr", "ptt",
            #"lgBNPP", "lgtrop", "trop99s", "trop99", "BNPPyn", "BNPyn", "BNPany",
            "mrale1st", "mralehigh", "mralelow", "sev_over_time",
            "cadLD", "cvdreg", "cvdhfLD", "cvdhfReg", "cvdafLD", "cvdafreg",
            "lavi", "ivcd", "pap", "lvef", "vr", "ar", "qrs", "qtc", "qt", "raxis", "taxis",
            "echo2", "vent", "intubated", "ecmo", "crrt", 
            "ED", "admit", "admitcovid", "valid", "admitvalid", "icu2",
            "dispo", "death"
            )


fVars <- c("ethnicity_MAR", "insurance", "pregnant", "after_july1",
           "asthma2", "copd2", "ckd2", "diabetes2", "prediabetes2", "htn2", "tob2", "hiv2", "obesity2", "CirrLiv2", "IschVasc2", "stroke2", "CF2", "HF2", "CVD2", "obesity2b", "tobcurr",
           "acei", "aceiprior", "arb", "arbprior", "asa", "asaprior", "ibuprof", "ibuprofprior", "nsaid", "nsaidprior", "med", "medhtn", "ccb", "bb", "diuretic", "aldo", "bpother", "medchol", "medstatin", "bldthin", "remdesivir", "solumedrol", "prednisone",
           #"lgBNPP", "lgtrop", "trop99s", "trop99", "BNPPyn", "BNPyn", "BNPany",
           "cadLD", "cvdreg", "cvdhfLD", "cvdhfReg", "cvdafLD", "cvdafreg",
           "echo2", "vent", "intubated", "ecmo", "crrt", 
           "ED", "admit", "admitcovid", "valid", "admitvalid", "icu2",
           "dispo", "death"
)

# "Table1" for whole data set
tab1 <- CreateTableOne(vars = myVars, factor = fVars, data = IncludedData, includeNA = TRUE)
tab1_bydeath <- CreateTableOne(vars = myVars, factor = fVars, strata = "death" , data = IncludedData, includeNA = TRUE)
tab1_byicu2 <- CreateTableOne(vars = myVars, factor = fVars, strata = "icu2" , data = IncludedData, includeNA = TRUE)
#print(tab1, showAllLevels = TRUE, formatOptions = list(big.mark = ","))
#print(tab1_bydeath, showAllLevels = TRUE, formatOptions = list(big.mark = ","))
tab1csv <- print(tab1, showAllLevels = TRUE, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
tab1_bydeathcsv <- print(tab1_bydeath, showAllLevels = TRUE, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
tab1_byicu2csv <- print(tab1_byicu2, showAllLevels = TRUE, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
## Save to a CSV file
write.csv(tab1csv, file = "Z:\\users\\abe_neuro\\RProjects\\All_Table1.csv")
write.csv(tab1_bydeathcsv, file = "Z:\\users\\abe_neuro\\RProjects\\All_Table1_bydeath.csv")
write.csv(tab1_byicu2csv, file = "Z:\\users\\abe_neuro\\RProjects\\All_Table1_byICU.csv")
# NA counts
na_count_mergedData <-sapply(IncludedData, function(y) sum(length(which(is.na(y)))))
na_count_mergedData <- data.frame(na_count_mergedData)
write.csv(na_count_mergedData,"Z:\\users\\abe_neuro\\RProjects\\All_NAcount.csv", row.names = TRUE)

# Similar Tables for people with verified admission for COVID (not just incidentally found to be COVID+)
Included_AdmitCovid <- IncludedData %>% filter(admitcovid==1)
ac_tab1 <- CreateTableOne(vars = myVars, factor = fVars, data = Included_AdmitCovid, includeNA = TRUE)
ac_tab1_bydeath <- CreateTableOne(vars = myVars, factor = fVars, strata = "death" , data = Included_AdmitCovid, includeNA = TRUE)
ac_tab1_byicu2 <- CreateTableOne(vars = myVars, factor = fVars, strata = "icu2" , data = Included_AdmitCovid, includeNA = TRUE)
ac_tab1csv <- print(ac_tab1, showAllLevels = TRUE, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
ac_tab1_bydeathcsv <- print(ac_tab1_bydeath, showAllLevels = TRUE, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
ac_tab1_byicu2csv <- print(ac_tab1_byicu2, showAllLevels = TRUE, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
## Save to a CSV file
write.csv(ac_tab1csv, file = "Z:\\users\\abe_neuro\\RProjects\\AdmitCovid_Table1.csv")
write.csv(ac_tab1_bydeathcsv, file = "Z:\\users\\abe_neuro\\RProjects\\AdmitCovid_Table1_bydeath.csv")
write.csv(ac_tab1_byicu2csv, file = "Z:\\users\\abe_neuro\\RProjects\\AdmitCovid_Table1_byICU.csv")
# NA counts
na_count_mergedData <-sapply(Included_AdmitCovid, function(y) sum(length(which(is.na(y)))))
na_count_mergedData <- data.frame(na_count_mergedData)
write.csv(na_count_mergedData,"Z:\\users\\abe_neuro\\RProjects\\AdmitCovid_NAcount.csv", row.names = TRUE)

# Similar Tables for people admitted specifically for COVID and who went to ICU
Included_AdmitCovid_ICU <- IncludedData %>% filter(admitcovid==1, icu2==1)
icu_tab1 <- CreateTableOne(vars = myVars, factor = fVars, data = Included_AdmitCovid_ICU, includeNA = TRUE)
icu_tab1_bydeath <- CreateTableOne(vars = myVars, factor = fVars, strata = "death" , data = Included_AdmitCovid_ICU, includeNA = TRUE)
icu_tab1csv <- print(icu_tab1, showAllLevels = TRUE, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
icu_tab1_bydeathcsv <- print(icu_tab1_bydeath, showAllLevels = TRUE, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
## Save to a CSV file
write.csv(icu_tab1csv, file = "Z:\\users\\abe_neuro\\RProjects\\AdmitCovidICU_Table1.csv")
write.csv(icu_tab1_bydeathcsv, file = "Z:\\users\\abe_neuro\\RProjects\\AdmitCovidICU_Table1_bydeath.csv")
# NA counts
na_count_mergedData <-sapply(Included_AdmitCovid_ICU, function(y) sum(length(which(is.na(y)))))
na_count_mergedData <- data.frame(na_count_mergedData)
write.csv(na_count_mergedData,"Z:\\users\\abe_neuro\\RProjects\\AdmitCovidICU_NAcount.csv", row.names = TRUE)


#7 #############################################################################
# Survival Analysis

# Below is just some preliminary code, not finalized by any means, more just to test how to run a survival analysis on this data

# Look at people who presented to ED
Included_ED <- IncludedData %>% filter(ED==1)

# Creating survival variables
Included_ED <- Included_ED %>% 
  #Create time_to_icu
  mutate(time_to_icu = as.numeric(difftime(icudate, indexstartdate, units = 'days'))) %>%
  mutate(time_to_icu = ifelse(time_to_icu==-1,0,time_to_icu)) %>% 
  #Create outcome variable, twodayicu(0/1)
  mutate(twodayicu = ifelse(time_to_icu<=2.0,1,0)) %>%
  mutate(twodayicu = replace_na(twodayicu, 0)) %>%
  #Clean indexenddate
  mutate(indexenddate2 = indexenddate) %>%
  mutate(indexenddate2 = coalesce(indexenddate2, icudate, indexstartdate)) %>%
  #Create time_to_death
  mutate(time_to_death = as.numeric(difftime(dod, indexstartdate, units='days'))) %>%
  mutate(time_to_studyend = as.numeric(difftime(indexenddate2, indexstartdate, units='days'))) %>%
  mutate(futime = coalesce(time_to_icu, time_to_death, time_to_studyend))

summary(Included_ED$covidage)

Included_ED <- Included_ED %>% mutate(age_cat = case_when(covidage < 35 ~ 'Under 35',
                                                        covidage >= 35 & covidage < 45 ~ '35-44',
                                                        covidage >= 45 & covidage < 55 ~ '45-54',
                                                        covidage >= 55 & covidage < 65 ~ '55-64',
                                                        covidage >= 65 & covidage < 75 ~ '65-74',
                                                        covidage >= 75 ~ '75+'))

summary(factor(Included_ED$age_cat))
summary(Included_ED$covidage)

# Confirm the below are 0 (did not die or reach study end before went to ICU (only for people who went to ICU tho))
sum(Included_ED$time_to_death<Included_ED$time_to_icu, na.rm=TRUE)
sum(Included_ED$time_to_studyend<Included_ED$time_to_icu, na.rm=TRUE)

# Checking some stats on survival time, futime has been adjusted to censor for death
summary(Included_ED$time_to_icu)
summary(Included_ED$time_to_death)
summary(Included_ED$time_to_studyend)
summary(Included_ED$futime)

# Create survival function
f1 <- survfit(Surv(futime, icu) ~ 1, data = Included_ED) #whole group
f2 <- survfit(Surv(futime, icu) ~ admitcovid, data = Included_ED) #stratified on admission for covid
f3 <- survfit(Surv(futime, icu) ~ age_cat, data = Included_ED) #stratified on age categories

# @ 2 days, survival = 89.6% so basically 11.4% admitted to ICU
surv_median(f1)

ggsurvplot(
  fit = f1, 
  xlab = "Days", 
  ylab = "Percent not admitted to ICU",
  xlim = c(0,30),
  break.time.by = 2,
  risk.table = T,
  ggtheme = theme_minimal(),
  risk.table.y.text.col = T,
  risk.table.y.text = FALSE
)
summary(f1)

ggsurvplot(
  fit = f3, 
  xlab = "Days", 
  ylab = "Percent not admitted to ICU",
  xlim = c(0,30),
  break.time.by = 2,
  risk.table = T,
  pval = T,
  #conf.int = T,
  ggtheme = theme_minimal(),
  risk.table.y.text.col = T,
  risk.table.y.text = FALSE
)


###########################################################3
# Below stuff is work in progress

Included_ED %>% filter(!is.na(mrale1st)) %>% group_by(twodayicu) %>% 
  summarize(n=n(), meanmrale=mean(mrale1st, na.rm=T))
Included_ED %>% filter(is.na(mrale1st)) %>% group_by(twodayicu) %>% 
  summarize(n=n(), meanmrale=mean(mrale1st, na.rm=T))

# Logistic regression, outcome = ICU within 2 days from indexstartdate
logit1 <- glm(twodayicu ~ covidage + female + obesity2b + aceiprior + cvdhfLD + sbp1st + dbp1st + pulse1st + osat1st + tempadmit + creat + hgb + wbc + anc + mrale1st + sev_over_time, family = binomial, data = Included_ED)
logistic.display(logit1)

#Extract MRNs for people with BNPP data
BNPPDF <- IncludedData %>% filter(!is.na(bnpp))
nrow(BNPPDF)
colnames(BNPPDF)
dplyr::count(BNPPDF, cvdhfLD, sort=TRUE)
#Grab just columns we need
BNPPDF <- dplyr::select(BNPPDF, id_str, coviddatefirst, cvdhfLD, indexstartdate, indexenddate)
BNPPDF <- left_join(BNPPDF, imgData2, by='id_str')
BNPPDF <- arrange(BNPPDF, id_str, studyDate)
# Arbitrarily set end date to end of data collection (9/8/2020), several of these patients have since been discharged or died
BNPPDF <- BNPPDF %>% mutate(indexenddate = replace_na(indexenddate, ymd("2020-09-08")))
BNPPDF <- BNPPDF %>% filter(between(studyDate, indexstartdate, indexenddate))
BNPPDF <- BNPPDF %>% filter(modality=="XR")
BNPPDF <- BNPPDF %>% group_by(id_str) %>% slice(1)
write.csv(BNPPDF, file = "Z:\\users\\abe_neuro\\RProjects\\BNPPDF_sample.csv")

# Extract complete list of phonetic IDs for people in the entire dataset
# imgData is Arterys-listing tab data
# imgData2 is the above, but with duplicate MRN/phonetic combos removed
# img_mRale_merge is the above with mRale scores merged in where available (no rows removed)
# daniels_imgmRale_merge is the above merged to daniels data (lose rows from arterys-listing that don't match an MRN in daniels data)

# filter for XRays only & no spine XRs (3396 obs)
Phonetic_Extract <- daniels_imgmRale_merge %>% 
  filter(modality=="XR", !grepl('SPINE', study_desc)) %>% 
  select(id_str, accn, phonetic_id, unique_phonetic, study_desc, studyDate)

imgPaths <- imgData %>% 
  select(id_str, accn, phonetic_id, unique_phonetic, study_desc, studyDate, modality, covid_any, available, localpath) %>% # get rid of extra columns
  arrange(id_str, phonetic_id, is.na(localpath)) %>%
  #arrange(id_str, phonetic_id, is.na(covid_any), is.na(available)) %>%  # sort so more complete row on top
  distinct(id_str, phonetic_id, .keep_all = TRUE) %>% # keep one
  mutate_at(c("studyDate", "covid_any"), mdy) %>% # convert these to date format
  arrange(id_str, studyDate, phonetic_id) %>% # sort by mrn, study date, and phonetic id
  select(unique_phonetic, localpath)

Phonetic_Extract <- left_join(Phonetic_Extract, imgPaths, by='unique_phonetic')
PhoneticNA <- filter(Phonetic_Extract, is.na(localpath))
Phonetic_toDL <- Phonetic_Extract %>% 
  filter(!grepl('covid-20201028', localpath)) %>%
  filter(!grepl('covid-20201019', localpath)) %>%
  filter(!grepl('covid-20200909', localpath))
write.csv(Phonetic_Extract,"Z:\\users\\abe_neuro\\RProjects\\Phonetic_Extract.csv", row.names = FALSE)
