# functions ====================================================================
locf <- function(x) { # Last Observation Carried Forward 
  bool <- !is.na(x)
  x[bool][cumsum(bool)]
}
locf2 <- function(x, id) { # locf() is not applicable for the vars that some ids lack a value in the first row
  ord <- order(id, x, na.last = TRUE)
  x2 <- x[ord]
  id2 <- id[ord] # pull the values to the first row
  head_rows <- tapply(seq_along(id2), id2, head, 1)
  x2 <- x2[head_rows]
  id2 <- id2[head_rows]
  x2[match(id, id2)]
}
# data =========================================================================
data <- readRDS("Data/napped_data_20241014")
data[data == ""] <- NA
data$study_id <- trimws(data$study_id) # remove leading and trailing whitespace " name", "name "
data$study_id <- locf(data$study_id)
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
bool <- data$redcap_data_access_group %in% "the_childrens_hosp"
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

#Change the number countries to names of the countries
africa_labels <- c("Other", "Algeria", "Angola", "Benin", "Botswana", "Burkina Faso",
                   "Burundi", "Cameroon", "Cape Verde", "Central African Republic",
                   "Chad", "Congo", "Egypt", "Morocco", "South Africa", "Tunisia")
asia_labels <- c(
  "Other", "Afghanistan", "Armenia", "Azerbaijan", "Bahrain", "Bangladesh",
  "Cambodia", "China", "Cyprus", "Georgia", "India", "Indonesia", "Iran",
  "Iraq", "Israel", "Japan", "Jordan", "Malaysia", "Nepal", "Oman",
  "Pakistan", "Philippines", "Qatar", "Saudi Arabia", "Singapore",
  "South Korea", "Thailand", "Turkey", "United Arab Emirates", "Vietnam"
)
europe_labels <- c(
  "Other", "Albania", "Austria", "Belgium", "Bulgaria", "Croatia",
  "Cyprus", "Czech Republic", "Denmark", "Estonia", "Finland", "France",
  "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Italy",
  "Latvia", "Luxemburg", "Netherlands", "Norway", "Poland", "Portugal",
  "Romania", "Russia", "Serbia", "Slovakia", "Slovenia", "Spain",
  "Sweden", "Switzerland", "Ukraine", "United Kingdom"
)
north_america_labels <- c(
  "Other", "Canada", "Costa Rica", "Cuba", "Dominica", "Dominican Republic",
  "El Salvador", "Guatemala", NA, NA, "Haïti", "Honduras", "Jamaica",
  "Mexico", "Nicaragua", "Panama", "United States of America"
)
south_america_labels <- c(
  "Other", "Argentina", "Bolivia", "Brazil", "Chile", "Colombia",
  "Ecuador", "Guyana", "Paraguay", "Peru", "Suriname", "Uruguay",
  "Venezuela"
)

data$country_name <- NA 
bool <- data$region2 %in% "0"
data$country_name[bool] <- africa_labels[as.numeric(data$c_africa2[bool])+1]
remove(bool)
bool <- data$region2 %in% "1"
data$country_name[bool] <- asia_labels[as.numeric(data$c_asia2[bool])+1]
remove(bool)
bool <- data$region2 %in% "3"
data$country_name[bool] <- europe_labels[as.numeric(data$c_europe2[bool])+1]
remove(bool)
bool <- data$region2 %in% "4"
data$country_name[bool] <- north_america_labels[as.numeric(data$c_n_america2[bool])+1]
remove(bool)
bool <- data$region2 %in% "5"
data$country_name[bool] <- south_america_labels[as.numeric(data$c_s_america2[bool])+1]
remove(bool)


vars2 <- c("date_of_birth_dte", "type_pfic_c.factor", "death_dte", 
           "date_of_diversion_dte", "date_of_liver_transplantat_dte",
           "patient_enrolled_in_drug_t_c",
           "medical_therapies_ever_app_c___5", 
           "medical_therapies_ever_app_c___6",
           "medical_therapies_ever_app_c___9",
           "medical_therapies_ever_app_c___0",
           "gender_c.factor",
           "mutations_on_first_allele.factor",
           "specification_mutation_fir",
           "mutations_on_second_allele.factor",
           "specification_mutation_sec"
)
data[, vars2] <- apply(data[, vars2, drop = FALSE], 2, locf2, data$study_id)
remove(vars2)
data$medical_therapies_ever_app_c___5 <- as.numeric(data$medical_therapies_ever_app_c___5)
data$medical_therapies_ever_app_c___6 <- as.numeric(data$medical_therapies_ever_app_c___6)
data$medical_therapies_ever_app_c___9 <- as.numeric(data$medical_therapies_ever_app_c___9)
data$medical_therapies_ever_app_c___0 <- as.numeric(data$medical_therapies_ever_app_c___0)
data$date_of_visit_dte <- as.Date(data$date_of_visit_dte) # encounter date
data$date_of_birth_dte <- as.Date(data$date_of_birth_dte) # date of birth
data$death_dte <- as.Date(data$death_dte) # date of death
data$date_of_diversion_dte <- as.Date(data$date_of_diversion_dte) # date of surgical biliary diversion (sbd)
data$date_of_liver_transplantat_dte <- as.Date(data$date_of_liver_transplantat_dte) # date of liver transplantation
data$date_cens <- pmin(data$death_dte, data$date_of_diversion_dte,
                       data$date_of_liver_transplantat_dte, na.rm = TRUE)
data$age_event <- difftime(data$date_cens, data$date_of_birth_dte, units = "days")
data$age_event <- as.numeric(data$age_event) / 365.25
data$age <- difftime(data$date_of_visit_dte, data$date_of_birth_dte, 
                     units = "days")
data$age <- as.numeric(data$age) / 365.25 # .25 to account for leap years
data$med_started___52 <- unlist(tapply(data$med_started___5, 
                                       match(data$study_id, unique(data$study_id)),
                                       cumsum))
data$id <- match(data$study_id, unique(data$study_id))
data$id <- as.numeric(data$id)
library(lubridate)
dte_18 <- data$date_of_birth_dte %m+% years(18)
dte_min <- pmin(data$death_dte, data$date_of_diversion_dte, 
                data$date_of_liver_transplantat_dte, dte_18, na.rm = TRUE)
data$dte_min <- dte_min
data$dte_min <- as.Date(data$dte_min)

data$event <- ifelse(!is.na(data$date_of_diversion_dte) & data$date_of_diversion_dte == data$dte_min, "SBD",
                     ifelse(!is.na(data$date_of_liver_transplantat_dte) & data$date_of_liver_transplantat_dte == data$dte_min, "LTx", 
                            ifelse(!is.na(data$death_dte) & data$death_dte == data$dte_min, "Death", "No")))
remove(dte_min, dte_18)


# platelets ====================================================================
## imput missing units ========================================================= 
# For hospitals that report only one unit, we assume this unit when the 
# measurement unit is missing.
tapply(data$plt_unit.factor, data$redcap_data_access_group,
       table, exclude = NULL)
data$plt_unit.factor2 <- data$plt_unit.factor
data$plt_unit_oth2 <- data$plt_unit_oth
bool1 <- is.na(data$plt_unit.factor2)
bool2 <- data$redcap_data_access_group %in% "aarhus_university"
data$plt_unit.factor2[bool1 & bool2] <- "10^9/L"
remove(bool2)
bool2 <- data$redcap_data_access_group %in% "al_jalila_children"
data$plt_unit.factor2[bool1 & bool2] <- "10^9/L"
remove(bool2)
bool2 <- data$redcap_data_access_group %in% "children_network"
data$plt_unit.factor2[bool1 & bool2] <- "10^9/L"
remove(bool2)
bool2 <- data$redcap_data_access_group %in% "fudan_university_s"
data$plt_unit.factor2[bool1 & bool2] <- "10^9/L"
remove(bool2)
bool2 <- data$redcap_data_access_group %in% "fundacion_cardioin"
data$plt_unit.factor2[bool1 & bool2] <- "10^9/L"
remove(bool2)
bool2 <- data$redcap_data_access_group %in% "hopitaux_universit"
data$plt_unit.factor2[bool1 & bool2] <- "10^9/L"
remove(bool2)
bool2 <- data$redcap_data_access_group %in% "hpital_de_la_timon"
data$plt_unit.factor2[bool1 & bool2] <- "10^9/L"
remove(bool2)
bool2 <- data$redcap_data_access_group %in% "innsbruck_medical"
data$plt_unit.factor2[bool1 & bool2] <- "10^9/L"
remove(bool2)
bool2 <- data$redcap_data_access_group %in% "institute_of_liver"
data$plt_unit.factor2[bool1 & bool2] <- "10^9/L"
remove(bool2)
bool2 <- data$redcap_data_access_group %in% "karolinska_institu"
data$plt_unit.factor2[bool1 & bool2] <- "10^9/L"
remove(bool2)
bool2 <- data$redcap_data_access_group %in% "rigshospitalet_cop"
data$plt_unit.factor2[bool1 & bool2] <- "10^9/L"
remove(bool2)
bool2 <- data$redcap_data_access_group %in% "shaare_zedek_medic"
data$plt_unit.factor2[bool1 & bool2] <- "10^9/L"
remove(bool2)
bool2 <- data$redcap_data_access_group %in% "sheikh_khalifa_med"
data$plt_unit.factor2[bool1 & bool2] <- "10^9/L"
remove(bool2)
bool2 <- data$redcap_data_access_group %in% "toronto_sick_kids"
data$plt_unit.factor2[bool1 & bool2] <- "10^9/L"
remove(bool2)
bool2 <- data$redcap_data_access_group %in% "university_medical"
data$plt_unit.factor2[bool1 & bool2] <- "10^9/L"
remove(bool2)
bool2 <- data$redcap_data_access_group %in% "vall_dhebron_unive"
data$plt_unit.factor2[bool1 & bool2] <- "10^9/L"
remove(bool2)
bool2 <- data$redcap_data_access_group %in% "johns_hopkins_chil"
data$plt_unit.factor2[bool1 & bool2] <- "10^3/μL"
remove(bool2)
bool2 <- data$redcap_data_access_group %in% "la_paz_university"
data$plt_unit.factor2[bool1 & bool2] <- "10^3/μL"
remove(bool2)
bool2 <- data$redcap_data_access_group %in% "meyer_childrens_ho"
data$plt_unit.factor2[bool1 & bool2] <- "10^3/μL"
remove(bool2)
bool2 <- data$redcap_data_access_group %in% "schneider_children"
data$plt_unit.factor2[bool1 & bool2] <- "10^3/μL"
remove(bool2)
bool2 <- data$redcap_data_access_group %in% "seoul_national_uni"
data$plt_unit.factor2[bool1 & bool2] <- "10^3/μL"
remove(bool2)
bool2 <- data$redcap_data_access_group %in% "the_childrens_merc"
data$plt_unit.factor2[bool1 & bool2] <- "10^3/μL"
remove(bool2)
bool2 <- data$redcap_data_access_group %in% "universittsklinikub"
data$plt_unit.factor2[bool1 & bool2] <- "10^3/μL"
remove(bool2)
bool2 <- data$redcap_data_access_group %in% "university_of_roch"
data$plt_unit.factor2[bool1 & bool2] <- "10^3/μL"
remove(bool2)
bool2 <- data$redcap_data_access_group %in% "aou_citta_della_sa"
data$plt_unit.factor2[bool1 & bool2] <- "mm3"
remove(bool2)
bool2 <- data$redcap_data_access_group %in% "hpital_denfants_de"
data$plt_unit.factor2[bool1 & bool2] <- "mm3"
remove(bool2)
bool2 <- data$redcap_data_access_group %in% "ntuch_taipei_taiwa"
data$plt_unit.factor2[bool1 & bool2] <- "k/uL"
remove(bool2)
remove(bool1)
tapply(data$plt_unit.factor2, data$redcap_data_access_group, table, exclude = NULL)
## plt unit ====================================================================
data$plt_c2 <- NA # plt in 10^9/L
bool <- data$plt_unit.factor2 %in% "10^9/L"
data$plt_c2[bool] <- data$plt_c[bool]  
remove(bool)
bool <- data$plt_unit.factor2 %in% "10^3/μL"
c <- 1 # 10^3/μL to 10^9/L
data$plt_c2[bool] <- data$plt_c[bool] * c
remove(bool, c)
bool <- data$plt_unit.factor2 %in% "k/uL"
c <- 1 # k/uL to 10^9/L
data$plt_c2[bool] <- data$plt_c[bool] * c
remove(bool, c)
bool <- data$plt_unit.factor2 %in% "mm3"
c <- 10^6/(10^9) # 1/mm3 to 10^9/L
data$plt_c2[bool] <- data$plt_c[bool] * c
remove(bool, c)
bool <- data$plt_unit_oth2 %in% "10^3/uL"
data$sba_c2[bool] <- data$sba_c[bool]
remove(bool)
bool <- data$plt_unit_oth2 %in% "cells/mm^3" | data$plt_unit_oth2 %in% "cells/mm3" 
c <- 10^6/(10^9) # 1/mm3 to 10^9/L
data$sba_c2[bool] <- data$sba_c[bool] * c
remove(bool, c)
# sba ==========================================================================
## imput missing units ========================================================= 
# For hospitals that report only one unit, we assume this unit when the 
# measurement unit is missing.
data$sba_unit.factor2 <- data$sba_unit.factor
data$sba_unit_oth2 <- data$sba_unit_oth
bool1 <- is.na(data$sba_unit.factor2)
bool2 <- data$redcap_data_access_group %in% "aarhus_university"
data$sba_unit.factor2[bool1 & bool2] <- "umol/L"
remove(bool2)
bool2 <- data$redcap_data_access_group %in% "al_jalila_children"
data$sba_unit.factor2[bool1 & bool2] <- "umol/L"
remove(bool2)
bool2 <- data$redcap_data_access_group %in% "aou_citta_della_sa"
data$sba_unit.factor2[bool1 & bool2] <- "umol/L"
remove(bool2)
bool2 <- data$redcap_data_access_group %in% "children_network"
data$sba_unit.factor2[bool1 & bool2] <- "umol/L"
remove(bool2)
bool2 <- data$redcap_data_access_group %in% "fudan_university_s"
data$sba_unit.factor2[bool1 & bool2] <- "umol/L"
remove(bool2)
bool2 <- data$redcap_data_access_group %in% "hpital_de_la_timon"
data$sba_unit.factor2[bool1 & bool2] <- "umol/L"
remove(bool2)
bool2 <- data$redcap_data_access_group %in% "hpital_denfants_de"
data$sba_unit.factor2[bool1 & bool2] <- "umol/L"
remove(bool2)
bool2 <- data$redcap_data_access_group %in% "hpital_neckerenfan"
data$sba_unit.factor2[bool1 & bool2] <- "umol/L"
remove(bool2)
bool2 <- data$redcap_data_access_group %in% "karolinska_institu"
data$sba_unit.factor2[bool1 & bool2] <- "umol/L"
remove(bool2)
bool2 <- data$redcap_data_access_group %in% "kings_college_hosp"
data$sba_unit.factor2[bool1 & bool2] <- "umol/L"
remove(bool2)
bool2 <- data$redcap_data_access_group %in% "meyer_childrens_ho"
data$sba_unit.factor2[bool1 & bool2] <- "umol/L"
remove(bool2)
bool2 <- data$redcap_data_access_group %in% "rigshospitalet_cop"
data$sba_unit.factor2[bool1 & bool2] <- "umol/L"
remove(bool2)
bool2 <- data$redcap_data_access_group %in% "schneider_children"
data$sba_unit.factor2[bool1 & bool2] <- "umol/L"
remove(bool2)
bool2 <- data$redcap_data_access_group %in% "semmelweis_univers"
data$sba_unit.factor2[bool1 & bool2] <- "umol/L"
remove(bool2)
bool2 <- data$redcap_data_access_group %in% "seoul_national_uni"
data$sba_unit.factor2[bool1 & bool2] <- "umol/L"
remove(bool2)
bool2 <- data$redcap_data_access_group %in% "shaare_zedek_medic"
data$sba_unit.factor2[bool1 & bool2] <- "umol/L"
remove(bool2)
bool2 <- data$redcap_data_access_group %in% "sheikh_khalifa_med"
data$sba_unit.factor2[bool1 & bool2] <- "umol/L"
remove(bool2)
bool2 <- data$redcap_data_access_group %in% "tcmhi_warsaw_polan"
data$sba_unit.factor2[bool1 & bool2] <- "umol/L"
remove(bool2)
bool2 <- data$redcap_data_access_group %in% "the_childrens_merc"
data$sba_unit.factor2[bool1 & bool2] <- "umol/L"
remove(bool2)
bool2 <- data$redcap_data_access_group %in% "toronto_sick_kids"
data$sba_unit.factor2[bool1 & bool2] <- "umol/L"
remove(bool2)
bool2 <- data$redcap_data_access_group %in% "umhatem_n_i_pirogo"
data$sba_unit.factor2[bool1 & bool2] <- "umol/L"
remove(bool2)
bool2 <- data$redcap_data_access_group %in% "universittsklinikub"
data$sba_unit.factor2[bool1 & bool2] <- "umol/L"
remove(bool2)
bool2 <- data$redcap_data_access_group %in% "university_hospita"
data$sba_unit.factor2[bool1 & bool2] <- "umol/L"
remove(bool2)
bool2 <- data$redcap_data_access_group %in% "university_medical"
data$sba_unit.factor2[bool1 & bool2] <- "umol/L"
remove(bool2)
bool2 <- data$redcap_data_access_group %in% "university_medicalc"
data$sba_unit.factor2[bool1 & bool2] <- "umol/L"
remove(bool2)
bool2 <- data$redcap_data_access_group %in% "ntuch_taipei_taiwa"
data$sba_unit.factor2[bool1 & bool2] <- "Other"
data$sba_unit_oth2[bool1 & bool2] <- "uM"
remove(bool2)
bool2 <- data$redcap_data_access_group %in% "hippokration_hospi"
data$sba_unit.factor2[bool1 & bool2] <- "Other"
data$sba_unit_oth2[bool1 & bool2] <- "μmol/L"
remove(bool2)
remove(bool1)
tapply(data$sba_unit.factor2, data$redcap_data_access_group, table, exclude = NULL)
## sba unit ====================================================================
mw <- 408.6 # molecular weight g/mol, https://pubchem.ncbi.nlm.nih.gov/compound/Bile-Acid
data$sba_c2 <- NA # sba in umol/L
bool <- data$sba_unit.factor2 %in% "umol/L"
data$sba_c2[bool] <- data$sba_c[bool]  
bool <- data$sba_unit.factor2 %in% "mg/dL"
c <- 0.001 / 0.1  # mg/dL to g/L
c <- c / mw # g/L to mol/L
c <- c * 10^6 # mol/L to µmol/L   
data$sba_c2[bool] <- data$sba_c[bool] * c
remove(bool, c)
bool <- data$sba_unit_oth2 %in% "microgram/L"
c <- 1/1000000 # ug/L to g/L
c <- c / mw # g/L to mol/L
c <- c * 10^6 # mol/L to µmol/L
data$sba_c2[bool] <- data$sba_c[bool] * c
remove(bool, c)
bool <- data$sba_unit_oth2 %in% "micromol/L" | data$sba_unit_oth2 %in% "μmol/L"
data$sba_c2[bool] <- data$sba_c[bool]
remove(bool)
remove(mw)
data$sba_c2[data$sba_c2 == 0] <- NA 
data$log_sba <- log(data$sba_c2)

# inclusion / exclusion ========================================================
usa <- c("children_network", "ddc_clinic_middlef", "johns_hopkins_chil", 
         "the_childrens_merc", "university_of_roch")
data <- data[!data$redcap_data_access_group %in% usa, ] # exclude individuals from USA hospitals 
remove(usa)
data <- data[data$type_pfic_c.factor %in% "BSEP deficiency (PFIC2)", ]
data <- data[!is.na(data$age), ]
data <- data[data$age <= 18, ] # exclude measurements collected at age above 18
data <- data[is.na(data$date_cens) | data$date_of_visit_dte < data$date_cens, ]# remove visits occurring on and after transplantation or surgical biliary diversion

bool <- !(data$medical_therapies_ever_app_c___5 | 
            data$medical_therapies_ever_app_c___6 | 
            data$medical_therapies_ever_app_c___9)
data <- data[bool, ] # remove individuals who had received Odevixibat, Maralixib, or Other IBAT inhibitors before arrival in your center
remove(bool)
bool <- !data$patient_enrolled_in_drug_t_c %in% 1
data <- data[bool, ] # remove individuals who ever enrolled in IBAT inhibitor drug trial 
remove(bool)
data <- data[!data$med_started___52, ] # remove encounters under IBAT treatment
quantile(data$plt_c2, probs = 0.95, na.rm = TRUE) # 883.4
data$plt_c2[data$plt_c2 > 900 & !is.na(data$plt_c2)] <- NA # exclude plt measurements above 900
quantile(data$sba_c2, probs = 0.95, na.rm = TRUE) # 527.9
data$log_sba[data$sba_c2 > 600 & !is.na(data$sba_c2)] <- NA
data <- data[!is.na(data$plt_c2) | !is.na(data$log_sba), ]
ids_plt <- unique(data$study_id[!is.na(data$plt_c2)]) # 375
ids_sba <- unique(data$study_id[!is.na(data$log_sba)]) # 308
data <- data[data$study_id %in% intersect(ids_plt, ids_sba), ]
remove(ids_plt, ids_sba)

# ADD BSEP COLUMNS
bsep_data <- read.csv2('C:/Users/HansenBE/OneDrive - UMCG/EASL 2025/BSEP data complete classes combined.csv')
bsep_data <- as.data.frame(bsep_data)
bsep_data$BSEPcat_BSEP13_BSEP1 <- as.factor(bsep_data$BSEPcat_BSEP13_BSEP1)
bsep_data$BSEPcat_BSEP13_BSEP3 <- as.factor(bsep_data$BSEPcat_BSEP13_BSEP3)
bsep_data$JHep_BSEP_cat <- as.factor(bsep_data$JHep_BSEP_cat)

merge_data_bsepcols <- merge(data, bsep_data[,c('count', 'JHep_BSEP_cat', 'BSEPcat_BSEP13_BSEP1','BSEPcat_BSEP13_BSEP3')], by = 'count', all.x = TRUE)
merge_data_bsepcols <- merge_data_bsepcols[!is.na(merge_data_bsepcols$BSEPcat_BSEP13_BSEP1), ]

data_bsepcol <- merge_data_bsepcols[merge_data_bsepcols$BSEPcat_BSEP13_BSEP1 != 0 & merge_data_bsepcols$BSEPcat_BSEP13_BSEP3 != 0, ]
data_notincl <- merge_data_bsepcols[merge_data_bsepcols$BSEPcat_BSEP13_BSEP1 == 0 & merge_data_bsepcols$BSEPcat_BSEP13_BSEP3 == 0, ]
length(unique(data_notincl$count)) # 111 patients of the 278 who were in the analysis are excluded

# descriptives =================================================================
length(unique(data_bsepcol$study_id)) # number of ids # 167 (old: 278)
sum(!is.na(data_bsepcol$plt_c2)) # number of plt measurements # 874 (old analysis: 1539)
sum(!is.na(data_bsepcol$log_sba)) # number of sba measurements # 624 (old: 1108)
quantile(rle(data_bsepcol$study_id[!is.na(data_bsepcol$plt_c2)])$lengths, probs = c(0.5, 0.25, 0.75)) # plt: 3   2   8
quantile(rle(data_bsepcol$study_id[!is.na(data_bsepcol$log_sba)])$lengths, probs = c(0.5, 0.25, 0.75)) # sba: 2   1   4
round(quantile(tapply(data_bsepcol$age, data_bsepcol$study_id, max), probs = c(0.5, 0.25, 0.75)), 2) # 3.46 1.07 9.35

# hlme model ===================================================================
library(lcmm)
library(splines)

### plt =====================================================================
bool <- !is.na(data_bsepcol$plt_c2)
round(quantile(data_bsepcol$age[bool], probs = c(0.10, 0.5, 0.90), na.rm = TRUE), 2) # 0.50  3.92  13.17
remove(bool)
lm1a_plt <- hlme(plt_c2 ~ ns(age, k = 3.92, B = c(0.50, 13.17)),
                 random =~ ns(age, k = 3.92, B = c(0.50, 13.17)), 
                 subject = "id", 
                 data = data_bsepcol) # ng = 1
saveRDS(lm1a_plt, "lm1a_plt_knots.rds")
lm2a_plt <- hlme(plt_c2 ~ ns(age, k = 3.92, B = c(0.50, 13.17)), 
                 random =~ ns(age, k = 3.92, B = c(0.50, 13.17)), 
                 subject = 'id', 
                 data = data_bsepcol, 
                 ng = 2, # 2 classes
                 mixture =~ ns(age, k = 3.92, B = c(0.50, 13.17)), 
                 B = lm1a_plt)
saveRDS(lm2a_plt, "lm2a_plt_knots.rds")
lm2a_plt_grid <- gridsearch(hlme(plt_c2 ~ ns(age, k = 3.92, B = c(0.50, 13.17)),
                                 random =~ ns(age, k = 3.92, B = c(0.50, 13.17)),
                                 subject = 'id',
                                 data = data_bsepcol,
                                 ng = 2, # 3 classes
                                 mixture =~ ns(age, k = 3.92, B = c(0.50, 13.17))),
                            rep = 100, maxiter = 30, minit = lm1a_plt)
saveRDS(lm2a_plt_grid, "lm2a_plt_grid_knots.rds")
lm3a_plt <- gridsearch(hlme(plt_c2 ~ ns(age, k = 3.92, B = c(0.50, 13.17)),
                            random =~ ns(age, k = 3.92, B = c(0.50, 13.17)),
                            subject = 'id',
                            data = data_bsepcol,
                            ng = 3, # 3 classes
                            mixture =~ ns(age, k = 3.92, B = c(0.50, 13.17))),
                       rep = 100, maxiter = 30, minit = lm1a_plt)
saveRDS(lm3a_plt, "lm3a_plt_knots.rds")
lm3a_plt <- readRDS("lm3a_plt_knots.rds")

summarytable(lm1a_plt, lm2a_plt_grid, lm3a_plt,
             which = c("G", "loglik", "conv", "npm", "AIC", "BIC", "SABIC", "entropy","ICL", "%class"))
summaryplot(lm1a_plt, lm2a_plt_grid, lm3a_plt, which = c("AIC", "BIC", "SABIC", "entropy","ICL"))

new_data <- data.frame(age = seq(0, 18, length.out = 50))
pred1a_plt <- predictY(lm1a_plt, new_data, var.time = "age", draws = TRUE)
pred2a_plt <- predictY(lm2a_plt, new_data, var.time = "age", draws = TRUE)
pred3a_plt <- predictY(lm3a_plt, new_data, var.time = "age", draws = TRUE)
par(mfrow = c(1, 3))
plot(pred1a_plt, col = 1, lty = 1, lwd = 2, ylab = expression(paste("Platelet count (", 10^9, "/L)")), xlab = "Age (years)",
     legend = NULL, main = "Predicted PLT trajectories (K = 1)", ylim = c(0, 800),
     shades = TRUE)
legend("topleft", legend = "Class 1", col = 1, lty = 1, lwd = 2, cex=0.8,seg.len = 1,x.intersp = 0.5, bty = "n")
plot(pred2a_plt, col = c("#ff7f0e", "#1f77b4"), lty = 1, lwd = 2, ylab = expression(paste("Platelet count (", 10^9, "/L)")), xlab = "Age (years)",
     legend = NULL, main = "Predicted PLT trajectories (K = 2)", ylim = c(0, 800),
     shades = TRUE)
legend("topleft", legend = c("Class 1", "Class 2"), col = c("#ff7f0e", "#1f77b4"), lty = 1, lwd = 2, cex=0.8, seg.len = 1,x.intersp = 0.5, bty = "n")
plot(pred3a_plt, col = c("#ff7f0e", "#1f77b4","#e377c2"), lty = 1, lwd = 2, ylab = expression(paste("Platelet count (", 10^9, "/L)")), xlab = "Age (years)",
     legend = NULL, main = "Predicted PLT trajectories (K = 3)", ylim = c(0, 800),
     shades = TRUE)
legend("topleft", legend = c("Class 1", "Class 2", "Class 3"), col = c("#ff7f0e", "#1f77b4","#e377c2"), lty = 1, lwd = 2, cex= 0.8,seg.len = 1,x.intersp = 0.5,bty = "n")

########## sba =====================================================================
bool <- !is.na(data_bsepcol$log_sba)
round(quantile(data_bsepcol$age[bool], probs = c(0.10, 0.5, 0.90), na.rm = TRUE), 2) # 0.41  3.66  13.01
remove(bool)
lm1a_sba <- hlme(log_sba ~ ns(age, k = 3.66, B = c(0.41, 13.01)),
                 random =~ ns(age, k = 3.66, B = c(0.41, 13.01)), 
                 subject = "id", 
                 data = data_bsepcol) # ng = 1
saveRDS(lm1a_sba, "lm1a_sba_knots.rds")
lm2a_sba <- gridsearch(hlme(log_sba ~ ns(age, k = 3.66, B = c(0.41, 13.01)),
                            random =~ ns(age, k = 3.66, B = c(0.41, 13.01)),
                            subject = 'id',
                            data = data_bsepcol,
                            ng = 2, # 2 classes
                            mixture =~ ns(age, k = 3.66, B = c(0.41, 13.01))),
                       rep = 100, maxiter = 30, minit = lm1a_sba)
saveRDS(lm2a_sba, "lm2a_sba_knots.rds")
lm3a_sba <- gridsearch(hlme(log_sba ~ ns(age, k = 3.66, B = c(0.41, 13.01)),
                            random =~ ns(age, k = 3.66, B = c(0.41, 13.01)),
                            subject = 'id',
                            data = data_bsepcol,
                            ng = 3, # 2 classes
                            mixture =~ ns(age, k = 3.66, B = c(0.41, 13.01))),
                       rep = 100, maxiter = 30, minit = lm1a_sba)
saveRDS(lm3a_sba, "lm3a_sba_knots.rds")

summarytable(lm1a_sba, lm2a_sba, lm3a_sba,
             which = c("G", "loglik", "conv", "npm", "AIC", "BIC", "SABIC", "entropy","ICL", "%class"))
summaryplot(lm1a_sba, lm2a_sba, lm3a_sba, which = c("AIC", "BIC", "SABIC", "entropy","ICL"))

new_data <- data.frame(age = seq(0, 18, length.out = 50))
pred1a_sba <- predictY(lm1a_sba, new_data, var.time = "age", draws = TRUE)
pred2a_sba <- predictY(lm2a_sba, new_data, var.time = "age", draws = TRUE)
pred3a_sba <- predictY(lm3a_sba, new_data, var.time = "age", draws = TRUE)
exp_pred1_sba <- pred1a_sba
exp_pred1_sba$pred <- exp(exp_pred1_sba$pred)
exp_pred2_sba <- pred2a_sba
exp_pred2_sba$pred <- exp(exp_pred2_sba$pred)
exp_pred3_sba <- pred3a_sba
exp_pred3_sba$pred <- exp(exp_pred3_sba$pred)

par(mfrow = c(1, 3))
plot(exp_pred1_sba, col = 1, lty = 1, lwd = 2, ylab = expression(paste("sBA (", mu, "mol/L)")), xlab = "Age (years)",
     legend = NULL, main = "Predicted sBA trajectories (K = 1)", ylim = c(0, 600),
     shades = TRUE)
legend("topleft", legend = "Class 1", col = 1, lty = 1, lwd = 2, cex=0.8,seg.len = 1,x.intersp = 0.5, bty = "n")
plot(exp_pred2_sba, col = c("#d62728", "#2ca02c"), lty = 1, lwd = 2, ylab = expression(paste("sBA (", mu, "mol/L)")), xlab = "Age (years)",
     legend = NULL, main = "Predicted sBA trajectories (K = 2)", ylim = c(0, 600),
     shades = TRUE)
legend("topleft", legend = c("Class 1", "Class 2"), col = c("#d62728", "#2ca02c"), lty = 1, lwd = 2, cex=0.8, seg.len = 1,x.intersp = 0.5, bty = "n")
plot(exp_pred3_sba, col = c("#d62728", "#2ca02c","#9467bd"), lty = 1, lwd = 2, ylab = expression(paste("sBA (", mu, "mol/L)")), xlab = "Age (years)",
     legend = NULL, main = "Predicted sBA trajectories (K = 3)", ylim = c(0, 600),
     shades = TRUE)
legend("topleft", legend = c("Class 1", "Class 2", "Class 3"), col = c("#d62728", "#2ca02c","#9467bd"), lty = 1, lwd = 2, cex= 0.8,seg.len = 1,x.intersp = 0.5,bty = "n")

## results =====================================================================
lm2a_plt <- readRDS("lm2a_plt_knots.rds")
lm2a_sba <- readRDS("lm2a_sba_knots.rds")

m_plt <- lm2a_plt
m_sba <- lm2a_sba
pprobs <- predictClass(m_plt, data_bsepcol) # post probabilities
data_bsepcol$class_plt <- pprobs$class[match(data_bsepcol$id, pprobs$id)]
remove(pprobs)
pprobs <- predictClass(m_sba, data_bsepcol) # post probabilities
data_bsepcol$class_sba <- pprobs$class[match(data_bsepcol$id, pprobs$id)]
remove(pprobs)

new_data <- data.frame(age = seq(0, 18, length.out = 1000))
pred_sba <- predictY(m_sba, new_data, var.time = "age", draws = TRUE)
pred_plt <- predictY(m_plt, new_data, var.time = "age", draws = TRUE)

# PLOTS ============================================================

# with original sba scale
exp_pred_sba <- pred_sba
exp_pred_sba$pred <- exp(exp_pred_sba$pred)

pdf("plot_trajectories_sba_knots_v3.pdf", width = 10, height = 7.5)
{
  par(cex.main = 1.5,  # title
      cex.lab = 1.3,   # axis labels
      cex.axis = 1.3, # axis tick labels  
      mar = c(5,6,4,2) + 0.1) # increase left margin
  
  plot(exp_pred_sba, col = c("#d62728", "#2ca02c"), lty = 1, lwd = 2, ylab = expression(paste("sBA (", mu, "mol/L)")), xlab = "Age (years)",
       main = "Predicted sBA trajectories", ylim = c(0, 600),
       shades = TRUE, las = 1,legend=NULL)
  legend("topright", legend = c("Class sBA1", "Class sBA2"),
         col = c("#d62728", "#2ca02c"), lty = 1, lwd = 2, cex = 1.4, bty = "n")
}
dev.off()

pdf("plot_trajectories_plt_knots_v3.pdf", width = 10, height = 7.5)
{
  par(cex.main = 1.5,  # title
      cex.lab = 1.3,   # axis labels
      cex.axis = 1.3, # axis tick labels  
      mar = c(5,6,4,2) + 0.1) # increase left margin 
  
  plot(pred_plt, col = c("#ff7f0e", "#1f77b4"), lty = 1, lwd = 2, ylab = expression(paste("Platelet count (", 10^9, "/L)")), xlab = "Age (years)", 
       main = "Predicted PLT trajectories", ylim = c(0, 800),
       shades = TRUE, las = 1,legend=NULL)
  legend("topright", legend = c("Class PLT1", "Class PLT2"),
         col = c("#1f77b4", "#ff7f0e"), lty = 1, lwd = 2, cex = 1.4, bty = "n")
}
dev.off()

# spaghetti plot combined classes sba with colours -------------------------
xlim <- range(data_bsepcol$age)
ylim <- range(exp(data_bsepcol$log_sba), na.rm = TRUE)
xlab <- "Age (years)"
ylab <- expression(paste("sBA (", mu, "mol/L)"))
pdf("spaghetti_sba_combined_classes_colours_v3.pdf", width = 10, height = 7.5)
{
  par(cex.main = 1.75,  # title
      cex.lab = 1.3,   # axis labels
      cex.axis = 1.3, # axis tick labels  
      mar = c(5,6,4,2) + 0.1) # increase left margin 
  
  # Class 1
  data_sba <- data_bsepcol[data_bsepcol$class_sba %in% c(1,2), ]
  bool <- !is.na(data_sba$log_sba)
  data_sba_notna <- data_sba[bool,]  
  
  data_class_1 <- data_bsepcol[data_bsepcol$class_sba == 1, ]
  bool <- !is.na(data_class_1$log_sba)
  data_class_1_notna <- data_class_1[bool,]
  
  plot(NA, xlim = xlim, ylim = ylim, xaxt = "n", yaxt = "n", axes = FALSE,
       xlab = xlab, ylab = ylab, main = 'Observed sBA measurements')
  mtext(paste0("(n=",length(unique(data_sba_notna$count))," PFIC2 patients)"),side =3, font=1,line=0.25,cex=1.5)
  box(lwd = 0.5)
  axis(1, col = NA, col.axis = 1, col.ticks = 1, lwd.ticks = 0.5)
  axis(2, col = NA, col.axis = 1, col.ticks = 1, lwd.ticks = 0.5, las = 2)
  cols <- c("#ff7f0e", "#1f77b4", "#2ca02c", "#d62728", "#d6272840", "#2ca02c40")
  spaghetti <- function(x, ...){
    lines(data_class_1_notna$age[x], exp(data_class_1_notna$log_sba[x]), col = cols[5], lwd = 0.05)
    points(data_class_1_notna$age[x], exp(data_class_1_notna$log_sba[x]), col = cols[5], pch = 16, cex = 1)
  } 
  invisible(tapply(seq_along(data_class_1_notna$count), data_class_1_notna$count, spaghetti, 
                   col = rgb(0, 0, 0, 0.5)))
  polygon(x = c(pred_sba$times$age, rev(pred_sba$times$age)), # class 1
          y = c(exp(pred_sba$pred[, 3]), rev(exp(pred_sba$pred[, 5]))),
          border = NA, col = adjustcolor(cols[4],alpha.f = 0.15))
  lines(pred_sba$times$age, exp(pred_sba$pred[, 1]), col = cols[4], lwd = 3) # class 1
  
  # Class 2
  data_class_2 <- data_bsepcol[data_bsepcol$class_sba == 2, ]
  bool <- !is.na(data_class_2$log_sba)
  data_class_2_notna <- data_class_2[bool,]
  
  spaghetti <- function(x, ...){
    lines(data_class_2_notna$age[x], exp(data_class_2_notna$log_sba[x]), col = cols[6], lwd = 0.05)
    points(data_class_2_notna$age[x], exp(data_class_2_notna$log_sba[x]), col = cols[6], pch = 16, cex = 1)
  } 
  invisible(tapply(seq_along(data_class_2_notna$count), data_class_2_notna$count, spaghetti, 
                   col = rgb(0, 0, 0, 0.5)))
  polygon(x = c(pred_sba$times$age, rev(pred_sba$times$age)), # class 2
          y = c(exp(pred_sba$pred[, 4]), rev(exp(pred_sba$pred[, 6]))),
          border = NA, col = adjustcolor(cols[3],alpha.f = 0.2))
  lines(pred_sba$times$age, exp(pred_sba$pred[, 2]), col = cols[3], lwd = 3) # class 2
  
  legend("topright", legend = c("Class sBA1", "Class sBA2"),
         col = c("#d62728", "#2ca02c"), lty = 1, lwd = 2, cex = 1.4, bty = "n")
  
}
dev.off()
remove(xlim, ylim, xlab, ylab, spaghetti, data_class, data_class_notna)

# spaghetti plot separate subgroups sba -------------------------------------------
xlim <- range(data_bsepcol$age)
ylim <- range(exp(data_bsepcol$log_sba), na.rm = TRUE)
xlab <- "Age (years)"
ylab <- expression(paste("sBA (", mu, "mol/L)"))
pdf("plt_traj_sba_knots_v3.pdf", width = 10, height = 7.5)
{
  par(cex.main = 1.5,  # title
      cex.lab = 1.3,   # axis labels
      cex.axis = 1.3, # axis tick labels  
      mar = c(5,6,4,2) + 0.1) # increase left margin 
  
  # Class 1
  data_class <- data_bsepcol[data_bsepcol$class_sba == 1, ]
  bool <- !is.na(data_class$log_sba)
  data_class_notna <- data_class[bool,]
  
  plot(NA, xlim = xlim, ylim = ylim, xaxt = "n", yaxt = "n", axes = FALSE,
       xlab = xlab, ylab = ylab, main = 'Observed Class sBA1 trajectory')
  mtext(paste0("(n=",length(unique(data_class_notna$count)),")"),side =3, font=1,line=0.5,cex=0.95)
  box(lwd = 0.5)
  axis(1, col = NA, col.axis = 1, col.ticks = 1, lwd.ticks = 0.5)
  axis(2, col = NA, col.axis = 1, col.ticks = 1, lwd.ticks = 0.5, las = 2)
  cols <- c("#ff7f0e", "#1f77b4", "#2ca02c", "#d62728")
  spaghetti <- function(x, ...){
    lines(data_class_notna$age[x], exp(data_class_notna$log_sba[x]), col = cols[4], lwd = 0.05)
    points(data_class_notna$age[x], exp(data_class_notna$log_sba[x]), col = cols[4], pch = 16, cex = 1)
  } 
  invisible(tapply(seq_along(data_class_notna$count), data_class_notna$count, spaghetti, 
                   col = rgb(0, 0, 0, 0.5)))
  polygon(x = c(pred_sba$times$age, rev(pred_sba$times$age)), # class 1
          y = c(exp(pred_sba$pred[, 3]), rev(exp(pred_sba$pred[, 5]))), 
          border = NA, col = rgb(0, 0, 0, alpha = 0.2))
  lines(pred_sba$times$age, exp(pred_sba$pred[, 1]), col = "black", lwd = 3) # class 1
  
  # Class 2
  data_class <- data_bsepcol[data_bsepcol$class_sba == 2, ]
  bool <- !is.na(data_class$log_sba)
  data_class_notna <- data_class[bool,]
  plot(NA, xlim = xlim, ylim = ylim, xaxt = "n", yaxt = "n", axes = FALSE,
       xlab = xlab, ylab = ylab, main = 'Observed Class sBA2 trajectory')
  mtext(paste0("(n=",length(unique(data_class_notna$count)),")"),side =3, font=1,line=0.5,cex=0.95)
  box(lwd = 0.5)
  axis(1, col = NA, col.axis = 1, col.ticks = 1, lwd.ticks = 0.5)
  axis(2, col = NA, col.axis = 1, col.ticks = 1, lwd.ticks = 0.5, las = 2)
  spaghetti <- function(x, ...){
    lines(data_class_notna$age[x], exp(data_class_notna$log_sba[x]), col = cols[3], lwd = 0.05)
    points(data_class_notna$age[x], exp(data_class_notna$log_sba[x]), col = cols[3], pch = 16, cex = 1)
  } 
  invisible(tapply(seq_along(data_class_notna$count), data_class_notna$count, spaghetti, 
                   col = rgb(0, 0, 0, 0.5)))
  polygon(x = c(pred_sba$times$age, rev(pred_sba$times$age)), # class 2
          y = c(exp(pred_sba$pred[, 4]), rev(exp(pred_sba$pred[, 6]))), 
          border = NA, col = rgb(0, 0, 0, alpha = 0.2))
  lines(pred_sba$times$age, exp(pred_sba$pred[, 2]), col = "black", lwd = 3) # class 2
  
}
dev.off()
remove(xlim, ylim, xlab, ylab, spaghetti, data_class, data_class_notna)

# spaghetti plot separate subgroups plt -------------------------------------------
xlim <- range(data_bsepcol$age)
ylim <- range(data_bsepcol$plt_c2, na.rm = TRUE)
ylim2 <- c(0, 800)
xlab <- "Age (years)"
ylab <- expression(paste("Platelet count (", 10^9, "/L)"))
pdf("spaghetti_plt_classes_traj_knots_v2.pdf", width = 10, height = 7.5)
{
  par(cex.main = 1.5,  # title
      cex.lab = 1.3,   # axis labels
      cex.axis = 1.3, # axis tick labels  
      mar = c(5,6,4,2) + 0.1) # increase left margin 
  
  # Class 1
  data_class <- data_bsepcol[data_bsepcol$class_plt == 1, ]
  bool <- !is.na(data_class$plt_c2)
  data_class_notna <- data_class[bool,]
  
  plot(NA, xlim = xlim, ylim = ylim2, xaxt = "n", yaxt = "n", axes = FALSE,
       xlab = xlab, ylab = ylab, main = 'Observed Class PLT2 trajectory')
  mtext(paste0("(n=",length(unique(data_class_notna$count)),")"),side =3, font=1,line=0.5,cex=0.95)
  box(lwd = 0.5)
  axis(1, col = NA, col.axis = 1, col.ticks = 1, lwd.ticks = 0.5)
  axis(2, col = NA, col.axis = 1, col.ticks = 1, lwd.ticks = 0.5,las=2)
  cols <- c("#ff7f0e", "#1f77b4", "#2ca02c", "#d62728")
  spaghetti <- function(x, ...){
    lines(data_class_notna$age[x], data_class_notna$plt_c2[x], col = cols[1], lwd = 0.05)
    points(data_class_notna$age[x], data_class_notna$plt_c2[x], col = cols[1], pch = 16, cex = 1)
  } 
  invisible(tapply(seq_along(data_class_notna$count), data_class_notna$count, spaghetti, 
                   col = rgb(0, 0, 0, 0.5)))
  polygon(x = c(pred_plt$times$age, rev(pred_plt$times$age)), # class 1
          y = c(pred_plt$pred[, 3], rev(pred_plt$pred[, 5])), 
          border = NA, col = rgb(0, 0, 0, alpha = 0.2))
  lines(pred_plt$times$age, pred_plt$pred[, 1], col = "black", lwd = 3) # class 1
  
  # Class 2
  data_class <- data_bsepcol[data_bsepcol$class_plt == 2, ]
  bool <- !is.na(data_class$plt_c2)
  data_class_notna <- data_class[bool,]
  
  plot(NA, xlim = xlim, ylim = ylim2, xaxt = "n", yaxt = "n", axes = FALSE,
       xlab = xlab, ylab = ylab, main = 'Observed Class PLT1 trajectory')
  mtext(paste0("(n=",length(unique(data_class_notna$count)),")"),side =3, font=1,line=0.5,cex=0.95)
  box(lwd = 0.5)
  axis(1, col = NA, col.axis = 1, col.ticks = 1, lwd.ticks = 0.5)
  axis(2, col = NA, col.axis = 1, col.ticks = 1, lwd.ticks = 0.5, las = 2)
  cols <- c("#ff7f0e", "#1f77b4", "#2ca02c", "#d62728")
  spaghetti <- function(x, ...){
    lines(data_class_notna$age[x], data_class_notna$plt_c2[x], col = cols[2], lwd = 0.05)
    points(data_class_notna$age[x], data_class_notna$plt_c2[x], col = cols[2], pch = 16, cex = 1)
  } 
  invisible(tapply(seq_along(data_class_notna$count), data_class_notna$count, spaghetti, 
                   col = rgb(0, 0, 0, 0.5)))
  polygon(x = c(pred_plt$times$age, rev(pred_plt$times$age)), # class 2
          y = c(pred_plt$pred[, 4], rev(pred_plt$pred[, 6])), 
          border = NA, col = rgb(0, 0, 0, alpha = 0.2))
  lines(pred_plt$times$age, pred_plt$pred[, 2], col = "black", lwd = 3) # class 2
  
}
dev.off()
remove(xlim, ylim, xlab, ylab, spaghetti, data_class, data_class_notna)
