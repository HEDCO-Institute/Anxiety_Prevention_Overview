library(here)
library(rio)
library(dplyr)
library(tidyverse)
library(metafor)
library(writexl)



## Import all datafiles -----------------------

# Outcome data
outd <- import(here("data", "APO_outcome_level.xlsx"))%>% 
  rename(refid = Refid) %>% 
  distinct(across(-User), .keep_all = TRUE)

# ES data
effect_df <- import(here("data", "APO_effect_level.csv"))%>% 
  janitor::clean_names() %>% 
  distinct(across(-user), .keep_all = TRUE) 

## Filter notes
# should have 395 effect sizes
# when export from filemaker remove empty rows (that 
# emerged due to matching with outcome level)

esd <- outd %>% 
  select(refid, Outcomes_k, study_outcome_type, study_outcome_domain) %>%
  rename(effect_outcome = Outcomes_k) %>% 
  right_join(effect_df, by = c("refid", "effect_outcome")) %>%
  # # here we remove Refid #10481 because no valid ES data
  filter(refid != 10481)
  

# RoB Level
# no filter needed - reconciled
robd <- import(here("data", "APO_study_ROB.xlsx")) %>%
  rename(refid = Refid)


# Group Level
# distinct used to reconcile
groupd <- import(here("data", "APO_group_level.xlsx"))%>%
  rename(refid = Refid) %>% 
  distinct(across(-User), .keep_all = TRUE) 
  

# Study Level
# no filter needed - reconciled

studyd <- import(here("data", "APO_study_level.xlsx")) %>%
  rename(refid = Refid)
  

## Explore outcome domains and K for each outcome ------

# List distinct values in outcome_domain and their frequencies
outcome_counts <- table(esd$study_outcome_domain)
outcome_counts_df <- as.data.frame(outcome_counts)
print(outcome_counts_df)

# Filter rows where effect_timing is 0 or greater; only taking
# the post-treatment ES


esd <- esd %>%
  filter(effect_timing >= 0)


esd <- esd %>%
  filter(is.na(effect_size_type_insufficient_information_to_calculate_effect_size))


# Count the number of K (Refid) within each study_outcome_domain 
outcome_counts_byRefID <- esd %>%
  group_by(study_outcome_domain) %>%
  summarise(count = n_distinct(refid))

# Print the result
print(outcome_counts_byRefID)

# 18 for Depression symptoms (we will remove 1)
# 6 for Well-Being 
# 8 for Anxiety diagnosis
# 32 for Anxiety symptoms (we will remove 1)
# 3 Educational achievement - no meta due to heterogeneous outcomes
# 1 Suicidal ideation - no meta

## Exploring Well-being outcome for plain meta-analysis--------------
# dependencies due to multiple timepoints, measures, and treatment contrasts
wbfilter <- esd %>%
  filter(study_outcome_domain == "Well-being") #%>%
  #filter(effect_timing == 0)

# made decision to remove Stallard which is a one-year follow-up


## Restructure ES data for meta-analytic file ----------------

## Creating moderators ----------------------------------------
# Use studyd or robd file, match via Refid (multiple match)

# 1. secondary schools versus primary (elementary) secondary_m

studyd <- studyd %>%
  rowwise() %>%
  mutate(secondary_m = case_when(
    any(grepl("\\bHigh School\\b", c_across(starts_with("study_school_level")), ignore.case = TRUE)) ~ "yes",
    any(grepl("\\bMiddle School\\b", c_across(starts_with("study_school_level")), ignore.case = TRUE)) ~ "yes",
    any(grepl("\\bSecondary School\\b", c_across(starts_with("study_school_level")), ignore.case = TRUE)) ~ "yes",
    any(grepl("\\bCannot tell\\b", c_across(starts_with("study_school_level")), ignore.case = TRUE)) ~ NA_character_,
    TRUE ~ "no"
  )) %>%
  ungroup() %>%
  mutate(secondary_m = factor(secondary_m, levels = c("no", "yes"), exclude = NULL)) %>%
  # Per Sean 4.12.2024 drawing from reports update to "no" for Refid 10096 and 10131
  mutate(secondary_m = case_when(
    refid %in% c(10096, 10131) ~ "no",
    TRUE ~ secondary_m
  ))

# 2. RCT moderator RCT_m


studyd <- studyd %>%
  mutate(RCT_m = case_when(
    study_research_design == "Randomized trial" ~ "yes",
    TRUE ~ "no"
  )) %>%
  mutate(RCT_m = factor(RCT_m, levels = c("no", "yes"), exclude = NULL))


# 3. study country Aust_m (Australia = yes)

studyd <- studyd %>%
  mutate(Aust_m = case_when(
    `study_country -> Australia` == "Australia" ~ "yes",
    TRUE ~ "no"
  )) %>%
  mutate(Aust_m = factor(Aust_m, levels = c("no", "yes"), exclude = NULL))

# 4. school public_m (public school = yes)

studyd <- studyd %>%
  mutate(public_m = case_when(
    `study_school_type -> Public` == "Public" ~ "yes",
    TRUE ~ "no"
  )) %>%
  # Per Sean 4.12.2024 drawing from reports update to "no" for Refid 10060
  mutate(public_m = case_when(
    refid %in% c(10060) ~ "no",
    TRUE ~ public_m
  )) %>%
  mutate(public_m = factor(public_m, levels = c("no", "yes"), exclude = NULL)) 



# 5. percent female 

studyd <- studyd %>%
  mutate(female_m = ifelse(study_percent_female == -999.000, NA, study_percent_female))

# 6. tau_comparator
# Note: this is computed at the row level within the esd data

esd <- esd %>%
  mutate(TAU_m = case_when(
    grepl("TAU", effect_comparison_group, ignore.case = TRUE) ~ "yes",
    TRUE ~ "no"
  )) %>%
  mutate(TAU_m = factor(TAU_m, levels = c("no", "yes"), exclude = NULL))


# 7. years since study published using 2024  
studyd <- studyd %>%
  mutate(study_publication_year = as.numeric(study_publication_year),
         year_m = 2024 - study_publication_year)



# 8. RoB high moderator

robd <- robd %>%
  mutate(rob_high_m = case_when(
    irob_overall_decision == "High" |
      crob_overall_judgment == "High" |
      robins_overall_judgment == "High" ~ "yes",
    TRUE ~ "no"
  ))

# 9. universal prevention moderator (universal = yes)

studyd <- studyd %>%
  mutate(universal_m = case_when(
         `study_prevention_level -> Universal` == "Universal" ~ "yes",
         TRUE ~ "no")) %>%
  mutate(public_m = factor(universal_m, levels = c("no", "yes"), exclude = NULL))


## Merge moderators and variables------------------------------------------------
# pull in all moderators and cluster trial variables from the studyd and robd via Refid match 
# year_m, female_m, public_m, Aust_m, universal_m, RCT_m, secondary_m

esd <- merge(esd, studyd[, c("refid", "year_m", "female_m", "public_m", "Aust_m", "RCT_m", "secondary_m", "study_assignment_level", "study_cluster_size", "universal_m")], by = "refid", all.x = TRUE)

esd <- merge(esd, robd[, c("refid", "rob_high_m")], by = "refid", all.x = TRUE)

# rearrange to compute the effect size adjustment for cluster trials 
esd <- esd %>%
  select(-effect_icc, study_cluster_size, everything()) %>%
  # update so that data are appropriately computed for individual non-cluster trials
  mutate(
    study_cluster_size = if_else(study_assignment_level == "Individual", 1, as.numeric(study_cluster_size)),
    effect_icc = if_else(study_assignment_level == "Individual", 0, effect_icc)
  )

# checking for the study_cluster_size missing 
# none missing -- not needed
# refid_list <- esd %>%
#   filter(study_cluster_size == -999.00) %>%
#   pull(refid)
# 
# print(refid_list)


## Input missing study_cluster_size-------------------------------------------------------------------
## see if this is resolved with new data and delete 
## code to input values computed from publication for study_cluster_size
# 10600 81.33 is study_cluster_size not pulled for some reason
# 10100 41.4 pulled from depression meta-analysis file

# esd <- esd %>%
#   mutate(study_cluster_size = case_when(
#     refid == 10600 &  study_cluster_size == -999) ~ 81.33,
#     refid == 10100 &  study_cluster_size == -999) ~ 41.4,
#     TRUE ~ study_cluster_size
#   ))



## Create design_effect var------------------------------------------------------


# First update effect_icc with reference below to 0.03 when missing
# reference used in this review (and depression meta)
# https://www.jclinepi.com/article/S0895-4356(23)00071-9/fulltext
esd <- esd %>%
  mutate(effect_icc = if_else(is.na(effect_icc) | effect_icc == -999.00, 0.03, effect_icc))

# Now compute design_effect
# this code is based on Cochrane formula
# https://training.cochrane.org/handbook/current/chapter-23#section-23-1-4


esd <- esd %>%
  mutate(design_effect = 1 + (study_cluster_size - 1) * effect_icc)

# We will use design_effect to adjust SE for any study_assignment_leve == "Cluster"

## update for dichotomous anxiety diagnosis outcome 



## ES Calc ---------------------------------------------------------

# First compute ES data for those effect sizes where M/SE presented
# Mean is copied over to Mean
# SE is converted to SD using this formula for each group
# SD = SE * sqrt(n)

esd1 <- esd %>%
  filter(effect_size_type_means_and_standard_errors_s_es == "Means and Standard Errors (SEs)") %>%
  mutate(
    effect_intervention_followup_mean = if_else(
      effect_size_type_means_and_standard_errors_s_es == "Means and Standard Errors (SEs)", 
      effect_se_intervention_mean,
      effect_intervention_followup_mean),
    effect_comparison_followup_mean = if_else(
      effect_size_type_means_and_standard_errors_s_es
      == "Means and Standard Errors (SEs)", 
      effect_se_comparison_mean, 
      effect_comparison_followup_mean),
    effect_intervention_followup_sd = if_else(effect_size_type_means_and_standard_errors_s_es == "Means and Standard Errors (SEs)",
                                              round(effect_se_intervention_se * sqrt(effect_intervention_followup_participants),2),
                                              effect_intervention_followup_sd),
    effect_comparison_followup_sd = if_else(effect_size_type_means_and_standard_errors_s_es == "Means and Standard Errors (SEs)",
                                            round(effect_se_comparison_se * sqrt(effect_comparison_followup_participants),2),
                                            effect_comparison_followup_sd
    )
  )

esd2 <- esd %>%
  filter(is.na(effect_size_type_means_and_standard_errors_s_es))

esd <- bind_rows(esd1, esd2)


# ANXIETY----------------------------------------------------------------
# SMD Anxiety Symptoms---------------------------------

esd_anxiety <- esd %>%
  filter(study_outcome_domain == "Anxiety symptoms")

## ensure that -999 or -999.00 are cleaned to NA

# Replace -999 and -999.00 with NA in the dataset
esd_anxiety[esd_anxiety == -999 | esd_anxiety == -999.00] <- NA
  

# unadjusted

anxiety_symptoms <- escalc(measure = "SMD",
              m1 = effect_intervention_followup_mean, sd1 = effect_intervention_followup_sd, n1 = effect_intervention_followup_participants,
              m2 = effect_comparison_followup_mean, sd2 = effect_comparison_followup_sd, n2 = effect_comparison_followup_participants,
              data = esd_anxiety)

# relocate order of yi and vi for meta-analytic file creation
# adjust SE for cluster trials

anxiety_symptoms <- anxiety_symptoms %>%
  mutate(se = sqrt(vi)) %>%
  mutate(sqrt_design_effect = sqrt(design_effect),  # step 1 Compute square root of design effect
         sea = se * sqrt_design_effect,
         via = sea^2# step 2 Multiply se with square root of design effect
  ) %>%
  select(yi, vi, via, se, sea, everything()) %>%
  # QEDs removed due to “critical” risk of bias: 10301, 10304, and 10349 
  filter(!(refid %in% c(10301, 10304, 10349)))

# to add from manual re-code 
# Refid 10135 to be "no for secondary_m
# rob_high_m should be: 
#	“yes” 10100 at 182 and 234 weeks (still “no” at 0, 26, 78, and 130 weeks) and 10330
#	“no” 10076 at 0 and 26 weeks (still “yes” at 78 weeks) and 10135 for the three child-reported estimates and parent-reported at 1 week (still “yes” for parents at 52 and 156 weeks)


# RR Anxiety Diagnosis---------------------------------------
# 8 studies so we will collapse the two treatment contrasts, and filter for most 
# proximal outcome
esd_anxiety_diagnosis <- esd %>%
  filter(study_outcome_domain == "Anxiety diagnosis") %>%
  filter(refid != 10062) 

esd_anxiety_diagnosis_trim <- esd_anxiety_diagnosis %>%
  filter(refid != 10114) %>%
  filter(refid != 10129) %>%
  group_by(refid) %>%
  slice(if (any(effect_timing == 0)) which.max(effect_timing == 0) else 1)

## remove dependencies because k = 8 

adiagnosis_filter1 <- esd_anxiety_diagnosis%>%
  filter(refid == 10114 & effect_timing == 0) %>%
  group_by(refid) %>%
  mutate(
    effect_intervention_n_successful = sum(effect_intervention_n_successful),
  #  effect_comparison_n_successful = sum(effect_comparison_n_successful),
    effect_intervention_n_unsuccessful = sum(effect_intervention_n_unsuccessful),
  #  effect_comparison_n_unsuccessful = sum(effect_comparison_n_unsuccessful),
    effect_intervention_followup_participants = sum(effect_intervention_followup_participants),
  #  effect_comparison_followup_participants = sum(effect_comparison_followup_participants),
    effect_intervention_followup_classrooms = sum(effect_intervention_followup_classrooms),
  #  effect_comparison_followup_classrooms = sum(effect_comparison_followup_classrooms),
    effect_intervention_group = "1-Lessons for Living: Think well, do well (Psychologist and Teacher led)"
  ) %>%
  slice(-2)

## updates to combine arms in Refid 10129

adiagnosis_filter2 <- esd_anxiety_diagnosis%>%
  filter(refid == 10129 & effect_timing == 0) %>%
  group_by(refid) %>%
  mutate(
    effect_intervention_n_successful = sum(effect_intervention_n_successful),
    #  effect_comparison_n_successful = sum(effect_comparison_n_successful),
    effect_intervention_n_unsuccessful = sum(effect_intervention_n_unsuccessful),
    #  effect_comparison_n_unsuccessful = sum(effect_comparison_n_unsuccessful),
    effect_intervention_followup_participants = sum(effect_intervention_followup_participants),
    #  effect_comparison_followup_participants = sum(effect_comparison_followup_participants),
    # effect_intervention_followup_classrooms = sum(effect_intervention_followup_classrooms),
    #  effect_comparison_followup_classrooms = sum(effect_comparison_followup_classrooms),
    effect_intervention_group = "1-FRIENDS - Psychologist and Teacher led"
  ) %>%
  slice(-2)

# append files together 
esd_anxiety_diagnosis <- bind_rows(esd_anxiety_diagnosis_trim,
                                   adiagnosis_filter1, adiagnosis_filter2)

## ensure that -999 or -999.00 are cleaned to NA

# Replace -999 and -999.00 with NA in the dataset
esd_anxiety_diagnosis[esd_anxiety_diagnosis == -999 | esd_anxiety_diagnosis == -999.00] <- NA

# compute RR for anxiety diagnosis
anxiety_diagnosis <- escalc(measure = "RR",
             ai = effect_intervention_n_unsuccessful, n1i = effect_intervention_followup_participants,
             ci = effect_comparison_n_unsuccessful, n2i = effect_comparison_followup_participants, 
             data = esd_anxiety_diagnosis)



# relocate order of yi and vi for meta-analytic file creation

anxiety_diagnosis <- anxiety_diagnosis %>%
  mutate(se = sqrt(vi)) %>%
  mutate(sqrt_design_effect = sqrt(design_effect),  # step 1 Compute square root of design effect
         sea = se * sqrt_design_effect,
         via = sea^2# step 2 Multiply se with square root of design effect
  ) %>%
  select(yi, vi, via, se, sea, everything()) 



# DEPRESSION---------------------------------------------
# SMD Depression-----------------------------------------

esd_depression <- esd %>%
  filter(study_outcome_domain == "Depression")

## ensure that -999 or -999.00 are cleaned to NA

# Replace -999 and -999.00 with NA in the dataset
esd_depression[esd_depression == -999 | esd_depression == -999.00] <- NA


# unadjusted

depression_symptoms <- escalc(measure = "SMD",
                           m1 = effect_intervention_followup_mean, sd1 = effect_intervention_followup_sd, n1 = effect_intervention_followup_participants,
                           m2 = effect_comparison_followup_mean, sd2 = effect_comparison_followup_sd, n2 = effect_comparison_followup_participants,
                           data = esd_depression)

# relocate order of yi and vi for meta-analytic dataset 

depression_symptoms <- depression_symptoms %>%
  mutate(se = sqrt(vi)) %>%
  mutate(sqrt_design_effect = sqrt(design_effect),  # step 1 Compute square root of design effect
         sea = se * sqrt_design_effect,
         via = sea^2# step 2 Multiply se with square root of design effect
  ) %>%
  select(yi, vi, via, se, sea, everything())


  # drop rows 46 and 47 given these are depression_diagnosis indicators (k = 1)
  
depression_symptoms <- depression_symptoms[-c(46, 47), ]

# add in from manual re-code, notes from Sean 
# 10135 - no for secondary_m
# RoB updates 
# rob_high_m should change for the following 
# 10076: no at 0 and 26 weeks
# 10100: yes at 182 and 234 weeks



# Well-being---------------------------------------------
# SMD Well-being-----------------------------------------

esd_wellbeing <- esd %>%
  filter(study_outcome_domain == "Well-being") %>%
  # remove per Sean notes about comparison being active
  filter(refid != 10062) %>%
  # remove per Sean notes about this being depression focused tx
  filter(effect_intervention_group != "2-Thiswayup Schools: Combating Depression") 

# Removing dependencies in the data because of low K 
esd_wellbeing_trim <- esd_wellbeing %>%
  filter(refid != 10121) %>%
  filter(refid != 10064) %>%
  group_by(refid) %>%
  slice(if (any(effect_timing == 0)) which.max(effect_timing == 0) else 1)

# Combine tx arms in Refid 10121 using formula in Cochrane handbook
# https://training.cochrane.org/handbook/current/chapter-06#section-6-5-2-10

wellbeing_filter1 <- esd_wellbeing %>%
  filter(refid == 10121 & effect_timing == 0) %>%
  mutate(
  effect_intervention_followup_mean = sum(effect_intervention_followup_mean * effect_intervention_followup_participants) / sum(effect_intervention_followup_participants),
  effect_intervention_followup_sd = sqrt(sum(((effect_intervention_followup_participants - 1) * effect_intervention_followup_sd^2 + 
                                  effect_intervention_followup_participants * (effect_intervention_followup_mean - effect_intervention_followup_mean)^2)) /
                             (sum(effect_intervention_followup_participants) - 1)),
  effect_intervention_followup_participants = sum(effect_intervention_followup_participants)
      ) %>%
  mutate(effect_intervention_group = "1-e-GAD school and health") %>%
  slice(-2)

# Combine tx arms in Refid 10064 using formula in Cochrane handbook
# https://training.cochrane.org/handbook/current/chapter-06#section-6-5-2-10

wellbeing_filter2 <- esd_wellbeing %>%
  filter(refid == 10064 & effect_timing == 52) %>%
  mutate(
    effect_intervention_followup_mean = sum(effect_intervention_followup_mean * effect_intervention_followup_participants) / sum(effect_intervention_followup_participants),
    effect_intervention_followup_sd = sqrt(sum(((effect_intervention_followup_participants - 1) * effect_intervention_followup_sd^2 + 
                                                  effect_intervention_followup_participants * (effect_intervention_followup_mean - effect_intervention_followup_mean)^2)) /
                                             (sum(effect_intervention_followup_participants) - 1)),
    effect_intervention_followup_participants = sum(effect_intervention_followup_participants)
  )%>%
  mutate(effect_intervention_group = "1-School and Health-led Friends") %>%
  slice(-2)

# append files together 
esd_wellbeing <- bind_rows(esd_wellbeing_trim,
                                   wellbeing_filter1, wellbeing_filter2)


## ensure that -999 or -999.00 are cleaned to NA

# Replace -999 and -999.00 with NA in the dataset
esd_wellbeing[esd_wellbeing == -999 | esd_wellbeing == -999.00] <- NA


# unadjusted

well_being <- escalc(measure = "SMD",
                              m1 = effect_intervention_followup_mean, sd1 = effect_intervention_followup_sd, n1 = effect_intervention_followup_participants,
                              m2 = effect_comparison_followup_mean, sd2 = effect_comparison_followup_sd, n2 = effect_comparison_followup_participants,
                              data = esd_wellbeing)

# reverse score 10060 for K6 because lower = better whereas all other ES higher = better
# in this domain 

well_being$yi <- ifelse(well_being$refid == 10060, -1 *well_being$yi, well_being$yi)

# relocate order of yi and vi for meta-analytic dataset 

well_being <- well_being %>%
  mutate(se = sqrt(vi)) %>%
  mutate(sqrt_design_effect = sqrt(design_effect),  # step 1 Compute square root of design effect
         sea = se * sqrt_design_effect,
         via = sea^2# step 2 Multiply se with square root of design effect
  ) %>%
  select(yi, vi, via, se, sea, everything())


# Educational achievement---------------------------------------------
# SMD Educational achievement-----------------------------------------

esd_edachieve <- esd %>%
  filter(study_outcome_domain == "Educational achievement")

## ensure that -999 or -999.00 are cleaned to NA

# Replace -999 and -999.00 with NA in the dataset
esd_edachieve[esd_edachieve == -999 | esd_edachieve == -999.00] <- NA


# unadjusted

ed_achieve <- escalc(measure = "SMD",
                     m1 = effect_intervention_followup_mean, sd1 = effect_intervention_followup_sd, n1 = effect_intervention_followup_participants,
                     m2 = effect_comparison_followup_mean, sd2 = effect_comparison_followup_sd, n2 = effect_comparison_followup_participants,
                     data = esd_edachieve)

# relocate order of yi and vi for meta-analytic dataset 

ed_achieve <- ed_achieve %>%
  mutate(se = sqrt(vi)) %>%
  mutate(sqrt_design_effect = sqrt(design_effect),  # step 1 Compute square root of design effect
         sea = se * sqrt_design_effect,
         via = sea^2# step 2 Multiply se with square root of design effect
  ) %>%
  select(yi, vi, via, se, sea, everything())


# Suicidal ideation---------------------------------------------
# RR Suicidal ideation-----------------------------------------

esd_si <- esd %>%
  filter(study_outcome_domain == "Suicidal ideation")

## ensure that -999 or -999.00 are cleaned to NA

# Replace -999 and -999.00 with NA in the dataset
esd_si[esd_si == -999 | esd_si == -999.00] <- NA


# unadjusted

suicidal_ideation <- escalc(measure = "RR",
                            ai = effect_intervention_n_unsuccessful, n1i = effect_intervention_followup_participants,
                            ci = effect_comparison_n_unsuccessful, n2i = effect_comparison_followup_participants, 
                            data = esd_si)

# relocate order of yi and vi for meta-analytic dataset 

suicidal_ideation <- suicidal_ideation %>%
  mutate(se = sqrt(vi)) %>%
  mutate(sqrt_design_effect = sqrt(design_effect),  # step 1 Compute square root of design effect
         sea = se * sqrt_design_effect,
         via = sea^2# step 2 Multiply se with square root of design effect
  ) %>%
  select(yi, vi, via, se, sea, everything())




# Save analysis files----------------------------------------------------------

# # Specify the file paths
# anxiety_file <- "Anxiety_Symptoms.xlsx"
# depression_file <- "Depression_Symptoms.xlsx"
# well_being_file <- "Well_being.xlsx"
# suicidal_ideation_file <- "Suicidal_ideation.xlsx"
# anxiety_diagnosis_file <- "Anxiety_diagnosis.xlsx"
# ed_achieve_file <- "Educational_achievement.xlsx"
# 
# # Write 'anxiety_symptoms' data to a new Excel file
# write_xlsx(anxiety_symptoms, path = anxiety_file)
# 
# # Write 'depression_symptoms' data to a new Excel file
# write_xlsx(depression_symptoms, path = depression_file)
# 
# # Write 'anxiety_diagnosis' data to a new Excel file
# write_xlsx(anxiety_diagnosis, path = anxiety_diagnosis_file)
# 
# # Write 'suicidal_ideation' data to a new Excel file
# write_xlsx(suicidal_ideation, path = ed_achieve_file)
# 
# # Write 'ed_achieve' data to a new Excel file
# write_xlsx(ed_achieve, path = suicidal_ideation_file)
# 
# # Write 'well_being' data to a new Excel file
# write_xlsx(well_being, path = well_being_file)


# Parking lot code -------------------------------------------------------------


# code to adjust for BL levels
# Not currently used as of 4.9.24

# Compute the differences between post-intervention and baseline means for each group
diff1 <- mean1 - baseline_mean1
diff2 <- mean2 - baseline_mean2

# Compute the adjusted means for each group (subtracting the baseline differences)
adj_mean1 <- mean1 - diff1
adj_mean2 <- mean2 - diff2





