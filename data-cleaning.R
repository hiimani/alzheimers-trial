# data-cleaning.R
# ***
# full code to create data set
# include potentially relevant baseline values to predict PCHG

library(tidyverse)
setwd("~/Desktop/math261a/project/")

### *1* read full data set. stored in "data" folder:
filenames <- list.files("data", pattern="*.csv", full.names = T)
rawdat <- lapply(filenames, read.csv)
dat <- lapply(rawdat, tail, -1) # save all data sets into list variable (dat)
datdesc <- lapply(rawdat, head, 1) # save variable descriptions in same format

# delete rownames and clean up names df names for easier access
dat <- lapply(dat, `rownames<-`, NULL)
names(datdesc) <- str_extract(basename(filenames), "[^.]+")
names(dat)  <- str_extract(basename(filenames), "[^.]+")

### *2* clean up chemistry (ADLBC) and hematology (ADLBH) blood test data
# we want to cast data in terms of PARAM (the specific test conducted)
# or PARAMCD, which is shortened.
# AVAL/BASE contain the raw blood test value that we want in our new columns
# LBNRIND tells us if the value is abnormal (choose to keep b/c the raw values 
# may be differently interpreted based on the patient's sex, weight, etc.)
# may not be important; in regression we will account for those things.

### *2.1* clean up chemistry blood test variables (ADLBC)
CHEM <- dat$ADLBC %>%
  filter(!startsWith(PARAMCD, "_")) %>%
  filter(AVISIT == "Baseline" | 
           (USUBJID == "01-703-1119" & AVISIT == ".") |
           (USUBJID == "01-708-1348" & AVISIT == "."))
# remove changes from previous visit and only keep values at baseline
# ensure that two patients (01-703-1119 and 01-708-1348) that do not have 
# explicit Baseline tests are included.
# length(unique(CHEM$USUBJID)) # == 254; good.

chemwide <- dcast(CHEM, USUBJID + AVISIT ~ PARAMCD, value.var = "AVAL")
chemrange <- dcast(CHEM, USUBJID + AVISIT ~ PARAMCD, value.var = "LBNRIND")
names(chemrange)[3:ncol(chemrange)] <- paste(
  names(chemrange)[3:ncol(chemrange)], "_REL", sep = "")
# put all lab results (both raw scores and relative interpretations) into 
# two new "wide" data sets, one blood test per column
# change the names of the columns in chemrange to reflect relative values

### *2.2* clean up hematology blood test variables (ADLBH)
# ADLBH (hematology) GOALS: 
dat$ADLBH
length(unique(dat$ADLBH$USUBJID)) # 254

HEM <- dat$ADLBH %>% 
  filter(!startsWith(PARAMCD, "_")) %>% 
  filter(AVISIT == "Baseline" |
           (USUBJID %in% c("01-708-1348", "01-703-1100") & VISITNUM == "1.2") |
           (USUBJID %in% c("01-703-1086", "01-703-1119", "01-703-1335", 
                           "01-709-1309", "01-711-1433") & AVISIT == ".")
  )
# 01-708-1348 has "unscheduled" visit b/w weeks 2 and 6 -- keep VISITNUM == 1.2
# (the other visit, week 5 i guess, is coded 5.1)
# 01-703-1100 has "two" baselines -- only keep the second (VISITNUM == 1.2)

hemwide <- dcast(HEM, USUBJID + AVISIT ~ PARAMCD, value.var = "AVAL")
hemrange <- dcast(HEM, USUBJID + AVISIT ~ PARAMCD, value.var = "LBNRIND")
names(hemrange)[3:ncol(hemrange)] <- paste(
  names(hemrange)[3:ncol(hemrange)], "_REL", sep = "")
# put all lab results (both raw scores and relative interpretations) into 
# two new "wide" data sets, one blood test per column
# change the names of the columns of hemrange to reflect relative values
# note that ANISO and POLYCHR have many missing values; likely poor regressors.



### *3* clean up ADAS-Cog Data (ADQSADAS)
# contains our variable of interest, PCHG
# we keep only variables from week 24/final retrieval to get PCHG values
# comparing week 24/final retrieval to baseline values 

adas <- dat$ADQSADAS %>% 
  filter(PARAMCD == "ACTOT" & AVISIT == "Week 24") %>% 
  filter( !(USUBJID %in% c("01-705-1292", "01-716-1189", "01-718-1250") & 
              !(VISIT %in% c("WEEK 24", "RETRIEVAL") )) )
# patients "01-705-1292", "01-716-1189", and "01-718-1250" have multiple
# "Week 24" AVISIT values. keep only WEEK 14 or RETRIEVAL VISIT values.
# keep only *full* adas-cog subscores. baseline values are included here.


### *4* merge all created data sets with ADSL
# ADSL contains patient demographic data; one patient per observation

# tidyverse merge; ensure no duplicate columns
adsl_adas <- intersect(names(dat$ADSL), names(adas))[-2]
adas2 <- select(adas, -all_of(adsl_adas))
df_list <- list(dat$ADSL, adas2, chemwide[-2], chemrange[-2], hemwide[-2], hemrange[-2])
# [-2] removes AVISIT from other data sets

# merge all data sets by patient id
fulldat <- df_list %>% reduce(full_join, by = 'USUBJID')


### *5* write full regression data set to cwd:
write_csv(fulldat, "pchg-data.csv")
