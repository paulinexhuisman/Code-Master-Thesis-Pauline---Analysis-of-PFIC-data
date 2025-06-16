final_data_eu_nw_sc <- readRDS("final_dataset_eu_nw_sc_final.rds")
library(dplyr)
# CALCULATE SURVIVAL TIMES
# Store the maximum dates of end_follow_up in a vector and the maximum date of visits
max_end_follow_up_date <- final_data_eu_nw_sc %>%
  group_by(count) %>%
  summarise(max_end_follow_up = max(end_follow_up, na.rm = TRUE))

max_visit_date <- final_data_eu_nw_sc %>%
  group_by(count) %>%
  summarise(max_date_visit = max(date_of_visit, na.rm = TRUE))

#Combine max end follow up with max visit date
max_end_dates <- max_end_follow_up_date %>%
  left_join(max_visit_date, by = c("count"))

max_end_dates$max_date_visit <- as.Date(max_end_dates$max_date_visit)
max_end_dates$max_end_follow_up <- as.Date(max_end_dates$max_end_follow_up)
max_end_dates$max_final_date <- pmax(max_end_dates$max_date_visit, max_end_dates$max_end_follow_up, na.rm = TRUE)

# remove the rows with an NA in one of the covariates used in PS model
covariates <- c("study_id","age", "gender", "tsb_conv", "alt_conv", "sba_conv")
final_data_eu_nw_sc <- final_data_eu_nw_sc[complete.cases(final_data_eu_nw_sc[ , covariates]), ]
saveRDS(final_data_eu_nw_sc, "final_data_eu_nw_sc.rds")

#### SELECT FIRST VISIT INDEX TIME
# Make column random baseline: The baseline has now been chosen randomly between the eligible visits. 
selected_random_baseline <- final_data_eu_nw_sc %>%
  filter(eligible == 1) %>%
  group_by(count) %>%
  slice_min(order_by = date_of_visit, n = 1) %>%
  mutate(random_baseline = 1) %>%
  mutate(
    #check if any of death, ltx, or sbd is 1
    event_occurred_per_patient = ifelse((death == 1 | liver_transpantation == 1 | sbd == 1) & !is.na(death) & !is.na(liver_transpantation) & !is.na(sbd), TRUE, FALSE),
    
    # if event occured, find minimum date
    relevant_date_event = ifelse(
      event_occurred_per_patient,
      pmin(date_death, date_ltx, date_sbd, na.rm = TRUE), #min date if event occurred
      NA
    ),
    event_type = case_when(
      is.na(relevant_date_event) ~ 4, # All dates are missing
      relevant_date_event == date_death ~ 1, # Smallest date is death
      relevant_date_event == date_ltx ~ 2, # Smallest date is ltx
      relevant_date_event == date_sbd ~ 3, # Smallest date is sbd
      TRUE ~ NA_integer_ # Catch-all for unexpected cases
    ) 
  )%>%
  ungroup()

data_st <- selected_random_baseline %>%
  left_join(max_end_dates %>% select(count, max_final_date), by = "count")

#Make column with the date of the event, when there has not been an event then the max(date final visit, end of follow up date) is used
#event_type 1 is death, 2, is ltx, 3 is sbd, 4 is censored (there has not been an event, or the date of an event has not been given)
data_st <- data_st %>%
  mutate(date_event = ifelse(is.na(relevant_date_event), as.character(max_final_date), as.character(relevant_date_event)))

dataset_survivaltime <- data_st %>%
  rename(date_0 = date_of_visit) %>%
  mutate(
    event_time = difftime(as.Date(date_event), as.Date(date_0),units="days"),
    status = ifelse(event_type == 4, 0, 1))

dataset_survivaltime <- dataset_survivaltime %>%
  mutate(event_time_years = as.numeric(event_time)/365.25)

# add log sba values 
dataset_survivaltime$log10_sba_conv <- log10(dataset_survivaltime$sba_conv)
dataset_survivaltime$log10_alt_conv <- log10(dataset_survivaltime$alt_conv)
dataset_survivaltime$log10_tsb_conv <- log10(dataset_survivaltime$tsb_conv)

# Save dataset_survivaltime as RDS 
saveRDS(dataset_survivaltime, "data_firstvisit_index_time_final.rds")

######### SELECT RANDOM INDEX TIME

# Make column random baseline: The baseline has now been chosen randomly between the eligible visits. 
selected_random_baseline <- final_data_eu_nw_sc %>%
  filter(eligible == 1) %>%
  group_by(count) %>%
  slice_sample(n = 1) %>%
  mutate(random_baseline = 1) %>%
  mutate(
    #check if any of death, ltx, or sbd is 1
    event_occurred_per_patient = ifelse((death == 1 | liver_transpantation == 1 | sbd == 1) & !is.na(death) & !is.na(liver_transpantation) & !is.na(sbd), TRUE, FALSE),
    
    # if event occured, find minimum date
    relevant_date_event = ifelse(
      event_occurred_per_patient,
      pmin(date_death, date_ltx, date_sbd, na.rm = TRUE), #min date if event occurred
      NA
    ),
    event_type = case_when(
      is.na(relevant_date_event) ~ 4, # All dates are missing
      relevant_date_event == date_death ~ 1, # Smallest date is death
      relevant_date_event == date_ltx ~ 2, # Smallest date is ltx
      relevant_date_event == date_sbd ~ 3, # Smallest date is sbd
      TRUE ~ NA_integer_ # Catch-all for unexpected cases
    ) 
  )%>%
  ungroup()

data_st <- selected_random_baseline %>%
  left_join(max_end_dates %>% select(count, max_final_date), by = "count")

#Make column with the date of the event, when there has not been an event then the max(date final visit, end of follow up date) is used
#event_type 1 is death, 2, is ltx, 3 is sbd, 4 is censored (there has not been an event, or the date of an event has not been given)
data_st <- data_st %>%
  mutate(date_event = ifelse(is.na(relevant_date_event), as.character(max_final_date), as.character(relevant_date_event)))

dataset_survivaltime <- data_st %>%
  rename(date_0 = date_of_visit) %>%
  mutate(
    event_time = difftime(as.Date(date_event), as.Date(date_0),units="days"),
    status = ifelse(event_type == 4, 0, 1))

dataset_survivaltime <- dataset_survivaltime %>%
  mutate(event_time_years = as.numeric(event_time)/365.25)

# add log sba values 
dataset_survivaltime$log10_sba_conv <- log10(dataset_survivaltime$sba_conv)
dataset_survivaltime$log10_alt_conv <- log10(dataset_survivaltime$alt_conv)
dataset_survivaltime$log10_tsb_conv <- log10(dataset_survivaltime$tsb_conv)

# Save dataset_survivaltime as RDS 
saveRDS(dataset_survivaltime, "data_random_index_time_1_final.rds")

#### SELECT LAST VISIT INDEX TIME
# Make column random baseline: The baseline has now been chosen randomly between the eligible visits. 
selected_random_baseline <- final_data_eu_nw_sc %>%
  filter(eligible == 1) %>%
  group_by(count) %>%
  slice_max(order_by = date_of_visit, n = 1) %>%
  mutate(random_baseline = 1) %>%
  mutate(
    #check if any of death, ltx, or sbd is 1
    event_occurred_per_patient = ifelse((death == 1 | liver_transpantation == 1 | sbd == 1) & !is.na(death) & !is.na(liver_transpantation) & !is.na(sbd), TRUE, FALSE),
    
    # if event occured, find minimum date
    relevant_date_event = ifelse(
      event_occurred_per_patient,
      pmin(date_death, date_ltx, date_sbd, na.rm = TRUE), #min date if event occurred
      NA
    ),
    event_type = case_when(
      is.na(relevant_date_event) ~ 4, # All dates are missing
      relevant_date_event == date_death ~ 1, # Smallest date is death
      relevant_date_event == date_ltx ~ 2, # Smallest date is ltx
      relevant_date_event == date_sbd ~ 3, # Smallest date is sbd
      TRUE ~ NA_integer_ # Catch-all for unexpected cases
    ) 
  )%>%
  ungroup()

data_st <- selected_random_baseline %>%
  left_join(max_end_dates %>% select(count, max_final_date), by = "count")

#Make column with the date of the event, when there has not been an event then the max(date final visit, end of follow up date) is used
#event_type 1 is death, 2, is ltx, 3 is sbd, 4 is censored (there has not been an event, or the date of an event has not been given)
data_st <- data_st %>%
  mutate(date_event = ifelse(is.na(relevant_date_event), as.character(max_final_date), as.character(relevant_date_event)))

dataset_survivaltime <- data_st %>%
  rename(date_0 = date_of_visit) %>%
  mutate(
    event_time = difftime(as.Date(date_event), as.Date(date_0),units="days"),
    status = ifelse(event_type == 4, 0, 1))

dataset_survivaltime <- dataset_survivaltime %>%
  mutate(event_time_years = as.numeric(event_time)/365.25)

# add log sba values 
dataset_survivaltime$log10_sba_conv <- log10(dataset_survivaltime$sba_conv)
dataset_survivaltime$log10_alt_conv <- log10(dataset_survivaltime$alt_conv)
dataset_survivaltime$log10_tsb_conv <- log10(dataset_survivaltime$tsb_conv)

# Save dataset_survivaltime as RDS 
saveRDS(dataset_survivaltime, "data_lastvisit_index_time_final.rds")
