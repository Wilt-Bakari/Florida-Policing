#fl incarcerated vs. incarcerated population chi sq test
pop <- as.matrix(FL_2016_pop[, c("Incarcerated", "Free")])
rownames(pop) <- FL_2016_pop$subject_race
chisq.test(pop)
results <- chisq.test(pop)

#fl population residuals
results$observed
results$expected
results$residuals
results$stdres

###############################################################################

#fl race vs. outcome chi sq test
library(dplyr)
library(tidyverse)
library(lubridate)

# in order to download the dataset (not available on github), here is the link: https://stacks.stanford.edu/file/druid:yg821jf8611/yg821jf8611_fl_statewide_2020_04_01.csv.zip
fl_statewide <- fl_statewide_2020_04_01 %>%
  select(date, county_name, subject_race, outcome)
fl_statewide[is.na(fl_statewide)] <- "unknown"
print(fl_statewide)

#narrowing data
fl_statewide$year <- format(as.Date(fl_statewide$date, format="%Y-%m-%d"), "%Y")
fl_statewide$date <- NULL
fl_2016<- fl_statewide %>%
  filter(year == 2016)
fl_2016 <- fl_2016 %>%
  mutate(subject_race = case_when(
    subject_race %in% c("other", "unknown") ~ "other/unknown",
    TRUE ~ subject_race))
fl_2016

#fl race vs outcome chi sq test
fl_freq <- table(fl_2016$subject_race,fl_2016$outcome)
fl_freq
rownames(fl_freq) <- c("Asian/NHPI", "Black","Hispanic", "AIAN/MR/Else","White")
colnames(fl_freq) <- c("Arrest", "Citation","Unknown", "Warning")
fl_freq <- fl_freq[order(rownames(fl_freq)), ]
fl_freq_transposed <- t(fl_freq)
fl_chisq_result <- chisq.test(fl_freq)

#fl outcome residuals
fl_chisq_result$observed
fl_chisq_result$expected
fl_chisq_result$stdres

###############################################################################

#orange county chi sq test
orange <- fl_2016 %>%
  filter(county_name == "Orange County" )
orange
orange_freq <- table(orange$subject_race,orange$outcome)
orange_freq
orange_chisq_result <- chisq.test(orange_freq)
orange_chisq_result

#orange outcome residuals
orange_chisq_result$observed
orange_chisq_result$expected
orange_chisq_result$residuals
orange_chisq_result$stdres

###############################################################################

#palm beach county chi sq test
palm_beach <- fl_2016 %>%
  filter(county_name == "Palm Beach County" )
pb_freq <- table(palm_beach$subject_race,palm_beach$outcome)
pb_freq
pb_freq_result <- chisq.test(pb_freq)
pb_freq_result

#pb outcome residuals
pb_freq_result$observed
pb_freq_result$expected
pb_freq_result$stdres

###############################################################################

#miami-dade county chi sq test
miami_dade <- fl_2016 %>%
  filter(county_name == "Miami-Dade County" )
miami_freq <- table(miami_dade$subject_race,miami_dade$outcome)
miami_freq
miami_chisq_result <- chisq.test(miami_freq)
miami_chisq_result

#miami outcome residuals
miami_chisq_result$observed
miami_chisq_result$expected
miami_chisq_result$residuals
miami_chisq_result$stdres

###############################################################################

#broward county chi sq test
broward <- fl_2016 %>%
  filter(county_name == "Broward County" )
broward_freq <- table(broward$subject_race,broward$outcome)
broward_freq
broward_chisq_result <- chisq.test(broward_freq)
broward_chisq_result

#broward outcome residuals
broward_chisq_result$observed
broward_chisq_result$expected
broward_chisq_result$stdres

###############################################################################

#hillsborough county chi sq test
hillsborough <- fl_2016 %>%
  filter(county_name == "Hillsborough County" )
hillsborough_freq <- table(hillsborough$subject_race,hillsborough$outcome)
hillsborough_freq
hillsborough_chisq_result <- chisq.test(hillsborough_freq)
hillsborough_chisq_result

#hillsborough outcome residuals
hillsborough_chisq_result$observed
hillsborough_chisq_result$expected
hillsborough_chisq_result$stdres




