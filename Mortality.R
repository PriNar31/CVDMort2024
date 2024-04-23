## mortality dataset PatientDF

## load packages
library(dplyr)
library(tidyverse)
library(data.table)
library(ggplot2)
library(readr)

## read mortality data
mor.data <- read.table("MOH-DataServices_mis5126_update.txt",sep = '|',header = T)

## remove duplicates (remove identical rows)
mor.data <- distinct(mor.data) %>%
  filter(!is.na(AGE_AT_DEATH))

## load event data
load("event.data.Rdata")

## remove duplicates
event.data <- distinct(event.data)

## filter event type with "IP"&"DT" (note, actually no event type DT)
event.data <- event.data %>%
  filter(EVENT_TYPE==c("IP","DT"))

## join event file and mortality dataset
mor.data <- mor.data %>%
  left_join(event.data,by="MASTER_ENCRYPTED_HCU_ID")

## load data
load("diags.10.Rdata")

## tidy datasets
diags.10 <- diags.10 %>%
  select("EVENT_ID",starts_with("diag_")) %>%
  pivot_longer(names_to = "codes", 
               cols = -EVENT_ID) %>%
  select(EVENT_ID,value) %>%
  rename(ICD_10_codes = value) %>%
  filter(ICD_10_codes!="")

## remove replicates
diags.10 <- distinct(diags.10)

## join diags.10
mor.data <- mor.data %>%
  left_join(diags.10,by="EVENT_ID")

## CVD mortality codes
CVD.mor.ICD10 <- c(paste0('I60',seq(0,9,1)),paste0('I6',seq(10,16,1)), 'I618', 'I619', paste0('E10',seq(50,53,1)), 'E1059',
                   
                   paste0('E11',seq(50,53,1)), 'E1159', paste0('E13',seq(50,53,1)), 'E1359',
                   
                   paste0('E14',seq(51,53,1)), 'E1459', paste0('G4',seq(50,53,1)), paste0('G4',seq(58,68,1)),
                   
                   'I110', 'I130', 'I132', 'I200', 'I201', paste0('I20',seq(8,9,1)),paste0('I2',seq(10,14,1)), 
                   
                   paste0('I2',seq(19,22,1)), paste0('I2',seq(28,36,1)), 'I238', 'I240', paste0('I2',seq(48,50,1)), 
                   
                   paste0('I25',seq(10,13,1)), paste0('I25',seq(3,6,1)), 'I258', 'I259', 'I460', 'I461', 'I469',
                   
                   'I50', 'I500', 'I501', 'I509', paste0('I6',seq(30,36,1)), 'I638', 'I639',
                   
                   'I64', paste0('I6',seq(50,53,1)), paste0('I6',seq(58,64,1)), paste0('I6',seq(68,70,1)), 'I672', 'I690',
                   
                   'I691', 'I693', 'I694', 'I698', 'I700', 'I701', paste0('I70',seq(21,24,1)),
                   
                   'I708', 'I709', paste0('I710',seq(0,3,1)), 'I711', paste0('I7',seq(13,16,1)), 'I718',
                   
                   paste0('I7',seq(39,45,1)), 'I748', 'I749')

## filter relevant codes
mor.data <- mor.data %>%
  filter(ICD_10_codes %in% CVD.mor.ICD10)

distinct(mor.data)

## function
sumPID_info <- function(patientID){

patientDF <- mor.data %>% filter(MASTER_ENCRYPTED_HCU_ID == patientID)

patientgender <- max(patientDF$GENDER.x, na.rm = TRUE)

year_min <- min(year(dmy(patientDF$EVENT_START_DATETIME)), na.rm = TRUE)
year_max <- max(year(dmy(patientDF$EVENT_START_DATETIME)), na.rm = TRUE)
year_diff <- year_max-year_min

age_min <- min(patientDF$AGE_AT_ADMISSION, na.rm = TRUE)
age_max <- max(patientDF$AGE_AT_ADMISSION, na.rm = TRUE)
age_diff <- age_max-age_min

# Define breaks for age groups (e.g., 0-9, 10-19, 20-29, ...)
breaks <- seq(0, min(patientDF$AGE_AT_DEATH) + 10, by = 10) - 1

# Create age groups using cut()
patientDF$age_group <- cut(patientDF$AGE_AT_DEATH, breaks = breaks, labels = paste0(breaks[-length(breaks)] + 1, "-", breaks[-1]))

# Assuming your dataframe is called patientDF and the column containing age is named "age"
agegroup <- max((as.character(patientDF$age_group)), na.rm = TRUE)

## check ethnic group
patientDF$ethnic_4 <- apply(patientDF[, c("ETHNICGP.x", "ETHNICG1.x", "ETHNICG2.x","ETHNICG3.x")], 1, function(x) any(grepl("^4\\d{1}$", x)))
patientDF$ethnic_3 <- apply(patientDF[, c("ETHNICGP.x", "ETHNICG1.x", "ETHNICG2.x","ETHNICG3.x")], 1, function(x) any(grepl("^3\\d{1}$", x)))
patientDF$ethnic_36 <- apply(patientDF[, c("ETHNICGP.x", "ETHNICG1.x", "ETHNICG2.x","ETHNICG3.x")], 1, function(x) any(grepl("^36$", x)))
patientDF$ethnic_43 <- apply(patientDF[, c("ETHNICGP.x", "ETHNICG1.x", "ETHNICG2.x","ETHNICG3.x")], 1, function(x) any(grepl("^43$", x)))
patientDF$ethnic_36_43 <- patientDF$ethnic_36 & patientDF$ethnic_43

ethnic_3 <- max((as.character(patientDF$ethnic_3)), na.rm = TRUE)
ethnic_4 <- max((as.character(patientDF$ethnic_4)), na.rm = TRUE)
ethnic_36 <- max((as.character(patientDF$ethnic_36)), na.rm = TRUE)
ethnic_43 <- max((as.character(patientDF$ethnic_43)), na.rm = TRUE)
ethnic_36_43 <- max((as.character(patientDF$ethnic_36_43)), na.rm = TRUE)




finalDF <- data.frame (patientID = patientID, 
                       patientgender = patientgender,
                       AGE_AT_DEATH = patientDF$AGE_AT_DEATH[1],
                       DATE_OF_DEATH = patientDF$DATE_OF_DEATH[1],
                       year_min = year_min,
                       year_max = year_max,
                       year_diff = year_diff,
                       age_min = age_min,
                       age_max = age_max,
                       age_diff = age_diff,
                       agegroup = agegroup,
                       ethnic_3 = ethnic_3,
                       ethnic_4 = ethnic_4,
                       ethnic_36 = ethnic_36,
                       ethnic_43 = ethnic_43,
                       ethnic_36_43 = ethnic_36_43)
}

# loop the entire PERSON_ID
avail_ID <- unique(mor.data$MASTER_ENCRYPTED_HCU_ID)

DF <- vector()
for(i in 1:length(avail_ID)){
  ID <- avail_ID[i]
  print(i)
  intDF <- sumPID_info(ID)
  DF <- rbind(DF, intDF)
}

DF <- DF %>%
  mutate(year_of_death = year (dmy(DATE_OF_DEATH)))

write_xlsx(DF,"C:\\Users\\64210\\Desktop\\Mortality.xlsx")