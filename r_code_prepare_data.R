library(dplyr)
library(tidyr)
library(ggplot2)

data <- readRDS("Data/napped_data_20241014")
data[data == ""] <- NA
data$study_id <- trimws(data$study_id) # remove leading and trailing whitespace " name", "name "
data$date_of_birth_dte[data$study_id %in% "OIR008"] <- NA # the data 2024-07-15 is not possible
data$date_of_visit_dte[data$date_of_visit_dte %in% "2002-04-15" & data$study_id %in% "KCH038"] <- NA # this visit occurs before birth
data$date_of_visit_dte[data$date_of_visit_dte %in% "2010-07-15" & data$study_id %in% "KCH056"] <- NA # this visit occurs before birth
data$date_of_visit_dte[data$date_of_visit_dte %in% "2000-09-15" & data$study_id %in% "CUSL 016"] <- NA # this visit occurs before birth

# Fill in regions and countries for all centers
bool <- data$redcap_data_access_group %in% "aarhus_university"
data$region2[bool] <- "3"
data$c_europe2[bool] <- "8"
remove(bool)
bool <- data$redcap_data_access_group %in% "agia_sofia_hospita"
data$region2[bool] <- "3"
data$c_europe2[bool] <- "13"
remove(bool)
bool <- data$redcap_data_access_group %in% "al_jalila_children"
data$region2[bool] <- "1"
data$c_asia2[bool] <- "28"
remove(bool)
bool <- data$redcap_data_access_group %in% "all_india_institut"
data$region2[bool] <- "1"
data$c_asia2[bool] <- "10"
remove(bool)
bool <- data$redcap_data_access_group %in% "aou_citta_della_sa"
data$region2[bool] <- "3"
data$c_europe2[bool] <- "17"
remove(bool)
bool <- data$redcap_data_access_group %in% "apollo_bgs_hospita"
data$region2[bool] <- "1"
data$c_asia2[bool] <- "10"
remove(bool)
bool <- data$redcap_data_access_group %in% "asan_medical_cente"
data$region2[bool] <- "1"
data$c_asia2[bool] <- "25"
remove(bool)
bool <- data$redcap_data_access_group %in% "asst_papa_giovanni"
data$region2[bool] <- "3"
data$c_europe2[bool] <- "17"
remove(bool)
bool <- data$redcap_data_access_group %in% "bictre_hospital_pa"
data$region2[bool] <- "3"
data$c_europe2[bool] <- "11"
remove(bool)
bool <- data$redcap_data_access_group %in% "birmingham_childre"
data$region2[bool] <- "3"
data$c_europe2[bool] <- "33"
remove(bool)
bool <- data$redcap_data_access_group %in% "brasilia_childrens"
data$region2[bool] <- "5"
data$c_s_america2[bool] <- "3"
remove(bool)
bool <- data$redcap_data_access_group %in% "children_network"
data$region2[bool] <- "4"
data$c_n_america2[bool] <- "16"
remove(bool)
bool <- data$redcap_data_access_group %in% "cliniques_universi"
data$region2[bool] <- "3"
data$c_europe2[bool] <- "3"
remove(bool)
bool <- data$redcap_data_access_group %in% "coimbra_university"
data$region2[bool] <- "3"
data$c_europe2[bool] <- "23"
remove(bool)
bool <- data$redcap_data_access_group %in% "ddc_clinic_middlef"
data$region2[bool] <- "4"
data$c_n_america2[bool] <- "16"
remove(bool)
bool <- data$redcap_data_access_group %in% "fudan_university_s"
data$region2[bool] <- "1"
data$c_n_asia2[bool] <- "7"
remove(bool)
bool <- data$redcap_data_access_group %in% "hippokration_hospi"
data$region2[bool] <- "3"
data$c_europe2[bool] <- "13"
remove(bool)
bool <- data$redcap_data_access_group %in% "hopitaux_universit"
data$region2[bool] <- "3"
data$c_europe2[bool] <- "31"
remove(bool)
bool <- data$redcap_data_access_group %in% "hospices_civils_de"
data$region2[bool] <- "3"
data$c_europe2[bool] <- "11"
remove(bool)
bool <- data$redcap_data_access_group %in% "hospital_dona_este"
data$region2[bool] <- "3"
data$c_europe2[bool] <- "23"
remove(bool)
bool <- data$redcap_data_access_group %in% "hospital_italiano"
data$region2[bool] <- "5"
data$c_s_america2[bool] <- "1"
remove(bool)
bool <- data$redcap_data_access_group %in% "hpital_de_la_timon"
data$region2[bool] <- "3"
data$c_europe2[bool] <- "11"
remove(bool)
bool <- data$redcap_data_access_group %in% "hpital_denfants_de"
data$region2[bool] <- "0"
data$c_africa2[bool] <- "15"
remove(bool)
bool <- data$redcap_data_access_group %in% "hpital_neckerenfan"
data$region2[bool] <- "3"
data$c_europe2[bool] <- "11"
remove(bool)
bool <- data$redcap_data_access_group %in% "innsbruck_medical"
data$region2[bool] <- "3"
data$c_europe2[bool] <- "2"
remove(bool)
bool <- data$redcap_data_access_group %in% "institute_of_liver"
data$region2[bool] <- "1"
data$c_asia2[bool] <- "10"
remove(bool)
bool <- data$redcap_data_access_group %in% "istanbul_universit"
data$region2[bool] <- "1"
data$c_asia2[bool] <- "27"
remove(bool)
bool <- data$redcap_data_access_group %in% "johns_hopkins_chil"
data$region2[bool] <- "4"
data$c_n_america2[bool] <- "16"
remove(bool)
bool <- data$redcap_data_access_group %in% "karolinska_institu"
data$region2[bool] <- "3"
data$c_europe2[bool] <- "30"
remove(bool)
bool <- data$redcap_data_access_group %in% "king_faisal_specia"
data$region2[bool] <- "1"
data$c_asia2[bool] <- "23"
remove(bool)
bool <- data$redcap_data_access_group %in% "kings_college_hosp"
data$region2[bool] <- "3"
data$c_europe2[bool] <- "33"
remove(bool)
bool <- data$redcap_data_access_group %in% "la_paz_university"
data$region2[bool] <- "3"
data$c_europe2[bool] <- "29"
remove(bool)
bool <- data$redcap_data_access_group %in% "meyer_childrens_ho"
data$region2[bool] <- "3"
data$c_europe2[bool] <- "17"
remove(bool)
bool <- data$redcap_data_access_group %in% "mh_istanbul_turkey"
data$region2[bool] <- "1"
data$c_asia2[bool] <- "27"
remove(bool)
bool <- data$redcap_data_access_group %in% "ntuch_taipei_taiwa"
data$region2[bool] <- "1"
data$c_asia2[bool] <- "7"
remove(bool)
bool <- data$redcap_data_access_group %in% "policlinico_milano"
data$region2[bool] <- "3"
data$c_europe2[bool] <- "17"
remove(bool)
bool <- data$redcap_data_access_group %in% "porto_alegre_brazi"
data$region2[bool] <- "5"
data$c_s_america2[bool] <- "3"
remove(bool)
bool <- data$redcap_data_access_group %in% "rigshospitalet_cop"
data$region2[bool] <- "3"
data$c_europe2[bool] <- "8"
remove(bool)
bool <- data$redcap_data_access_group %in% "semmelweis_univers"
data$region2[bool] <- "3"
data$c_europe2[bool] <- "14"
remove(bool)
bool <- data$redcap_data_access_group %in% "seoul_national_uni"
data$region2[bool] <- "1"
data$c_asia2[bool] <- "25"
remove(bool)
bool <- data$redcap_data_access_group %in% "shaare_zedek_medic"
data$region2[bool] <- "1"
data$c_asia2[bool] <- "14"
remove(bool)
bool <- data$redcap_data_access_group %in% "sheikh_khalifa_med"
data$region2[bool] <- "1"
data$c_asia2[bool] <- "28"
remove(bool)
bool <- data$redcap_data_access_group %in% "tcmhi_warsaw_polan"
data$region2[bool] <- "3"
data$c_europe2[bool] <- "22"
remove(bool)
bool <- data$redcap_data_access_group %in% "the_childrens_merc"
data$region2[bool] <- "4"
data$c_n_america2[bool] <- "16"
remove(bool)
bool <- data$redcap_data_access_group %in% "toronto_sick_kids"
data$region2[bool] <- "4"
data$c_n_america2[bool] <- "1"
remove(bool)
bool <- data$redcap_data_access_group %in% "umhatem_n_i_pirogo"
data$region2[bool] <- "3"
data$c_europe2[bool] <- "4"
remove(bool)
bool <- data$redcap_data_access_group %in% "university_hospita"
data$region2[bool] <- "3"
data$c_europe2[bool] <- "17"
remove(bool)
bool <- data$redcap_data_access_group %in% "university_medical"
data$region2[bool] <- "3"
data$c_europe2[bool] <- "20"
remove(bool)
bool <- data$redcap_data_access_group %in% "university_medicalb"
data$region2[bool] <- "3"
data$c_europe2[bool] <- "20"
remove(bool)
bool <- data$redcap_data_access_group %in% "university_medicalc"
data$region2[bool] <- "3"
data$c_europe2[bool] <- "28"
remove(bool)
bool <- data$redcap_data_access_group %in% "university_of_roch"
data$region2[bool] <- "4"
data$c_n_america2[bool] <- "16"
remove(bool)
bool <- data$redcap_data_access_group %in% "vall_dhebron_unive"
data$region2[bool] <- "3"
data$c_europe2[bool] <- "29"
remove(bool)
bool <- data$redcap_data_access_group %in% "the_children_hosp"
data$region2[bool] <- "4"
data$c_n_america2[bool] <- "1"
remove(bool)
bool <- data$redcap_data_access_group %in% "universittsklinikub"
data$region2[bool] <- "3"
data$c_europe2[bool] <- "12"
remove(bool)
bool <- data$redcap_data_access_group %in% "universittskliniku"
data$region2[bool] <- "3"
data$c_europe2[bool] <- "12"
remove(bool)


new_data <- data.frame(count = data[,1], study_id = data[['study_id']], site = data[['redcap_data_access_group']], date_of_birth = data[['date_of_birth_dte']], date_of_visit = data[['date_of_visit_dte']], age = data[['age_calc']],
                       weight = data[['body_weight_c']], gender = data[['gender_c']], type_pfic = data[['type_pfic_c']], sba = data[['sba_c']], sba_unit = data[['sba_unit']], sba_unit_oth = data[['sba_unit_oth']], sba_uln = data[['sba_uln_c']], bric_phenotype = data[['initial_bric_phenotype_c']],
                       liver_transpantation = data[['liver_transplantation_c']], date_ltx = data[['date_of_liver_transplantat_dte']], sbd = data[['sbd']], date_sbd = data[['date_of_diversion_dte']],
                       death = data[['death_c']], date_death = data[['death_dte']], end_follow_up = data[['end_of_follow_up_c']],
                       hcc = data[['diag_hcc']], date_hcc = data[['hcc_dte']], hcc_2 = data[['hcc_2_c']], date_hcc_2 = data[['date_of_diagnosis_hcc_2_c']],  alt = data[['alt_c']], alt_unit = data[['alt_unit']], alt_uln = data[['alt_uln_c']], tsb = data[['tsb_c']], 
                       tsb_unit = data[['tsb_unit']], tsb_uln = data[['tsb_uln_c']], ggt = data[['ggt_c']], ggt_unit = data[['ggt_unit']], ggt_uln = data[['ggt_uln_c']], alb = data[['alb']], alb_unit = data[['alb_unit']], alb_uln = data[['alb_uln']], 
                       plt = data[['plt_c']], plt_unit = data[['plt_unit']], plt_uln = data[['plt_uln_c']], medication = data[['medical_therapies_ever_app_c___5']],
                       region2 = data[['region2']], c_africa2 = data[['c_africa2']], c_asia2 = data[['c_asia2']], 
                       c_europe2 = data[['c_europe2']], c_n_america2 = data[['c_n_america2']], c_s_america2 = data[['c_s_america2']])


# Fill missing values in dataset, for the values which have the same values per visit
new_data <- new_data %>%
  group_by(count) %>%
  mutate(
    date_of_birth = first(date_of_birth),
    date_ltx = first(date_ltx),
    date_sbd = first(date_sbd),
    date_death = first(date_death)
  ) %>%
  fill(alt_uln, .direction = "down") %>%
  fill(tsb_uln, .direction = "down") %>%
  fill(ggt_uln, .direction = "down") %>%
  fill(plt_uln, .direction = "down") %>%
  fill(alb_uln, .direction = "down") %>%
  fill(sba_uln, .direction = "down") %>%
  fill(study_id, .direction = "down")%>%
  fill(type_pfic, .direction = "down")%>%
  fill(liver_transpantation, .direction = "down") %>%
  fill(sbd, .direction = "down")%>%
  fill(death, .direction = "down")%>%
  fill(gender, .direction = "down")%>%
  mutate(
    alt_unit = ifelse(
      is.na(alt_unit) & n_distinct(na.omit(alt_unit)) == 1, # Check if all non-missing values are the same
      first(na.omit(alt_unit)), # fill with the common value
      alt_unit
    ),
    tsb_unit = ifelse(
      is.na(tsb_unit) & n_distinct(na.omit(tsb_unit)) == 1, # Check if all non-missing values are the same
      first(na.omit(tsb_unit)), # fill with the common value
      tsb_unit
    ),
    ggt_unit = ifelse(
      is.na(ggt_unit) & n_distinct(na.omit(ggt_unit)) == 1, # Check if all non-missing values are the same
      first(na.omit(ggt_unit)), # fill with the common value
      ggt_unit
    ),
    plt_unit = ifelse(
      is.na(plt_unit) & n_distinct(na.omit(plt_unit)) == 1, # Check if all non-missing values are the same
      first(na.omit(plt_unit)), # fill with the common value
      plt_unit
    ),
    alb_unit = ifelse(
      is.na(alb_unit) & n_distinct(na.omit(alb_unit)) == 1, # Check if all non-missing values are the same
      first(na.omit(alb_unit)), # fill with the common value
      alb_unit
    ),
    sba_unit = ifelse(
      is.na(sba_unit) & n_distinct(na.omit(sba_unit)) == 1, # Check if all non-missing values are the same
      first(na.omit(sba_unit)), # fill with the common value
      sba_unit
    ),
    sba_unit_oth = ifelse(
      is.na(sba_unit_oth) & n_distinct(na.omit(sba_unit_oth)) == 1, # Check if all non-missing values are the same
      first(na.omit(sba_unit_oth)), # fill with the common value
      sba_unit_oth
    )
  ) %>%
  ungroup()


# Fill missing alt_uln values with the most frequent value within each hospital
new_data <- new_data %>%
  group_by(site) %>%
  mutate(
    alt_uln = as.numeric(alt_uln),
    # Check if all values in alt_uln are NA
    most_frequent_alt_uln = ifelse(all(is.na(alt_uln)), NA, 
                                   as.numeric(names(sort(table(alt_uln), decreasing = TRUE)[1]))),
    # Replace NA values in alt_uln with the most frequent value, if available
    alt_uln2 = ifelse(is.na(alt_uln), most_frequent_alt_uln, alt_uln)
  ) %>%
  ungroup() %>%
  select(-most_frequent_alt_uln)  # Remove the temporary column
sum(is.na(new_data$alt_uln2))


# Fill missing tsb_uln values with the most frequent value within each hospital
new_data <- new_data %>%
  group_by(site) %>%
  mutate(
    tsb_uln = as.numeric(tsb_uln),
    # Check if all values in tsb_uln are NA
    most_frequent_tsb_uln = ifelse(all(is.na(tsb_uln)), NA, 
                                   as.numeric(names(sort(table(tsb_uln), decreasing = TRUE)[1]))),
    # Replace NA values in tsb_uln with the most frequent value, if available
    tsb_uln2 = ifelse(is.na(tsb_uln), most_frequent_tsb_uln, tsb_uln)
  ) %>%
  ungroup() %>%
  select(-most_frequent_tsb_uln)  # Remove the temporary column


# Fill missing sba_uln values with the most frequent value within each hospital
new_data <- new_data %>%
  group_by(site) %>%
  mutate(
    sba_uln = as.numeric(sba_uln),
    # Check if all values in sba_uln are NA
    most_frequent_sba_uln = ifelse(all(is.na(sba_uln)), NA, 
                                   as.numeric(names(sort(table(sba_uln), decreasing = TRUE)[1]))),
    # Replace NA values in sba_uln with the most frequent value, if available
    sba_uln2 = ifelse(is.na(sba_uln), most_frequent_sba_uln, sba_uln)
  ) %>%
  ungroup() %>%
  select(-most_frequent_sba_uln)  # Remove the temporary column
sum(is.na(new_data$sba_uln2))
#View(data[c('count', 'sba_c', 'sba_uln_c', 'sba_unit', 'sba_uln2')])


#ONLY GET THE DATA FROM REGIONS EUROPE 3
new_data_eu <- new_data[new_data$region2 %in% c('3'), ]

# When still missing (so missing in the whole site)
# Fill missing alt_uln values with the most frequent value within countries in EU
new_data_eu <- new_data_eu %>%
  group_by(c_europe2) %>%
  mutate(
    # Check if all values in alt_uln are NA
    most_frequent_alt_uln = ifelse(all(is.na(alt_uln2)), NA, 
                                   as.numeric(names(sort(table(alt_uln2), decreasing = TRUE)[1]))),
    # Replace NA values in alt_uln with the most frequent value, if available
    alt_uln2 = ifelse(is.na(alt_uln2), most_frequent_alt_uln, alt_uln2)
  ) %>%
  ungroup() %>%
  select(-most_frequent_alt_uln)  # Remove the temporary column

# When still missing (so missing in the whole site)
# Fill missing tsb_uln values with the most frequent value within countries in EU
new_data_eu <- new_data_eu %>%
  group_by(c_europe2) %>%
  mutate(
    # Check if all values in tsb_uln are NA
    most_frequent_tsb_uln = ifelse(all(is.na(tsb_uln2)), NA, 
                                   as.numeric(names(sort(table(tsb_uln2), decreasing = TRUE)[1]))),
    # Replace NA values in tsb_uln with the most frequent value, if available
    tsb_uln2 = ifelse(is.na(tsb_uln2), most_frequent_tsb_uln, tsb_uln2)
  ) %>%
  ungroup() %>%
  select(-most_frequent_tsb_uln)  # Remove the temporary column

# When still missing (so missing in the whole site)
# Fill missing sba_uln values with the most frequent value within countries in EU
new_data_eu <- new_data_eu %>%
  group_by(c_europe2) %>%
  mutate(
    # Check if all values in sba_uln are NA
    most_frequent_sba_uln = ifelse(all(is.na(sba_uln2)), NA, 
                                   as.numeric(names(sort(table(sba_uln2), decreasing = TRUE)[1]))),
    # Replace NA values in sba_uln with the most frequent value, if available
    sba_uln2 = ifelse(is.na(sba_uln2), most_frequent_sba_uln, sba_uln2)
  ) %>%
  ungroup() %>%
  select(-most_frequent_sba_uln)  # Remove the temporary column


# SPLIT DATASET EU IN NORTHERN, WESTERN, SOUTHERN, CENTRAL EUROPE
new_data_eu_west <- new_data_eu[new_data_eu$c_europe2 %in% c('3','11','16','19','20','33'), ]
new_data_eu_south <- new_data_eu[new_data_eu$c_europe2 %in% c('1','6','13','17','23','29'), ]
new_data_eu_central <- new_data_eu[new_data_eu$c_europe2 %in% c('2','4','5','7','12','14','18','22','24','25','26','27','28','31','32'), ]
new_data_eu_north <- new_data_eu[new_data_eu$c_europe2 %in% c('8','9','10','15','21','30'), ]

# COMBINE TO EU WEST_NORTH AND EU SOUTH_CENTRAL
new_data_eu_west_north <- rbind(new_data_eu_west, new_data_eu_north)
new_data_eu_south_central <- rbind(new_data_eu_south, new_data_eu_central)

# COMBINE TWO DATASETS WITH ADDED COLUMN NORTH_WEST OR SOUTH_CENTRAL
# NW = NORTH & WEST EUROPE = 1 and SC = SOUTH & CENTRAL EUROPE = 0
new_data_eu_comb <- rbind(new_data_eu_west_north, new_data_eu_south_central)
new_data_eu_comb$nw_or_sc <- ifelse(new_data_eu_comb$c_europe2 %in% c('3','11','16','19','20','33','8','9','10','15','21','30'), '1',
                                    ifelse(new_data_eu_comb$c_europe2 %in% c('1','6','13','17','23','29','2','4','5','7','12','14','18','22','24','25','26','27','28','31','32'),'0',NA))
remove(new_data_eu_central,new_data_eu_north,new_data_eu_south, new_data_eu_west)
remove(new_data_eu_south_central, new_data_eu_west_north)

# Calculate ages in dataset
calculate_age <- function(date_1, date_2){
  # Ensure inputs are date objects
  date_1 <- as.Date(date_1)
  date_2 <- as.Date(date_2)
  age <- as.numeric(difftime(date_2,date_1,units = "days")) / 365.25
  return(age)
}

new_data_eu_comb <- new_data_eu_comb %>%
  mutate(
    age = ifelse(is.na(age), calculate_age(date_of_birth,date_of_visit), age),
    ltx_before_day1 = ifelse((liver_transpantation == 1 & as.Date(date_ltx) <= as.Date(date_of_visit)) | 
                               (liver_transpantation == 1 & date_of_visit == "" ) |
                               (liver_transpantation == 1 & date_ltx == ""), 1, 0),
    ltx_before_day1 = ifelse(liver_transpantation == 0 | liver_transpantation == 2 | is.na(liver_transpantation), 0, ltx_before_day1),
    sbd_before_day1 = ifelse((sbd == 1 & as.Date(date_sbd) <= as.Date(date_of_visit)) |
                               (sbd == 1 & date_of_visit == "") |
                               (sbd == 1 & date_sbd == ""), 1, 0),
    sbd_before_day1 = ifelse(sbd == 0 | sbd == 2 | is.na(sbd), 0, sbd_before_day1),
    alt_conv = alt/alt_uln2,
    tsb_conv = tsb/tsb_uln2,
    sba_conv = sba/sba_uln2
  )

#Order visit dates, make columns first visit and last visit, make column with diff in consecutive visits
new_data_eu_comb_ordered <- new_data_eu_comb %>%
  group_by(count) %>%
  arrange(date_of_visit, .by_group = TRUE) %>%                                                  # Order visit dates
  mutate(visit = row_number()) %>% 
  mutate(
    first_visit = ifelse(visit == 1, 1, 0),                                                     # Make columns first visit and last visit
    last_visit = ifelse(visit == max(visit), 1, 0)
  ) %>%
  mutate(
    diff_previous_visit = as.integer(as.Date(date_of_visit) - lag(as.Date(date_of_visit)))) %>% # Make diff in consecutive visits
  ungroup()

# Make eligible columns
new_data_eu_comb_ordered <- new_data_eu_comb_ordered %>%
  mutate(
    eligible_pfic = ifelse(type_pfic == 0 | type_pfic == 1, 1, 0),
    eligible_age = ifelse(age >= 1 & age <= 18, 1, 0),
    eligible_ltx = ifelse(ltx_before_day1 == 0, 1, 0),
    eligible_sbd = ifelse(sbd_before_day1 == 0, 1, 0),
    eligible_alt = ifelse(alt_conv <= 15 & !is.na(alt_conv), 1, 0),
    eligible_sba = ifelse(sba_conv >= 3 & !is.na(sba_conv), 1, 0),
    eligible_diag1990 =  ifelse(date_of_visit >= as.Date("1991-01-01"), 1, 0)
  )

# Eind eligible criteria toevoegen
new_data_eu_comb_ordered <- new_data_eu_comb_ordered %>%
  mutate(
    eligible = ifelse(eligible_pfic == 1 & 
                        eligible_age == 1 & 
                        eligible_ltx == 1 & 
                        eligible_sbd == 1 & 
                        eligible_alt == 1 & 
                        eligible_sba == 1 &
                        eligible_diag1990 == 1
                      , 1, 0)
  )

eligible_data_eu_comb <- new_data_eu_comb_ordered %>%
  filter(eligible == 1)

#Save final dataset with eligible column
saveRDS(new_data_eu_comb_ordered, "final_dataset_eu_nw_sc_final.rds")
