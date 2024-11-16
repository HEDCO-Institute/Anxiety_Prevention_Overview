#install and load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, rio, here, readxl, bib2df, stringi, DT, openxlsx)

######################### DATA CLEANING ########################################

## Import ###################################################
#review eligibility
review_elig_df <- import(here("data", "APO_review_eligibility.xlsx"))

#study eligibility (no linked refs)
study_elig_df <- import(here("data", "APO_study_eligibility.xlsx")) %>%
  janitor::clean_names()

#review level and rob
review_df <- import(here("data", "APO_review_data_collection.xlsx")) %>%
  janitor::clean_names()

#study citation matrix
#set path to eligibility data
elig_path <- here("data", "Anxiety_Overview_Eligibility_Data.xlsx")

#import excel primary study reference sheet
#ps_allreferences <- read_excel(elig_path, sheet = "reports")

#import excel citation matrix (with header to calculate # of included/eligible studies)
ps_cm <- read_excel(elig_path, sheet = "citation_matrix")

#import excel citation matrix sheet (without header to transpose for lists of included/eligible reviews)
citation_matrix <- read_excel(elig_path, sheet = "citation_matrix", col_names = FALSE)

#study level
study_df <- import(here("data", "APO_study_level.xlsx")) %>%
  janitor::clean_names()

# #ROB_level
# study_rob_df <- import(here("data", "APO_study_ROB.xlsx"))%>%
#   janitor::clean_names()

#group_level
group_df <- import(here("data", "APO_group_level.xlsx"))%>%
  janitor::clean_names()

#outcome_level
outcome_df <- import(here("data", "APO_outcome_level.xlsx"))%>%
  janitor::clean_names()

#effect_level
effect_df <- import(here("data", "APO_effect_level.csv"))%>%
  janitor::clean_names()

#import linked citations for all references
linked_ref_df <- import(here("data", "APO_linked_references.csv")) %>%
  janitor::clean_names()

#import external links
public_links <- import(here("data", "APO_public_links.csv")) %>%
  janitor::clean_names() %>%
  select(refid, starts_with("link")) %>%
  mutate(link_author = na_if(link_author, ""))

## Tidy #######################################################################

#study level
inc_ps_wlinked <- study_elig_df %>%
  filter(study_eligibility_decision == "Eligible") %>%
  pull(refid)

inc_ps <- study_df %>%
  pull(refid)

study_td <- study_df %>%
  rowwise() %>%
  mutate(study_grade_level = paste(na.omit(c_across(starts_with("study_grade_level_"))), collapse = "; "),
         study_school_level = paste(na.omit(c_across(starts_with("study_school_level_"))), collapse = "; "),
         study_country = paste(na.omit(c_across(starts_with("study_country_"))), collapse = "; "),
         study_school_area = paste(na.omit(c_across(starts_with("study_school_area_"))), collapse = "; "),
         study_school_type = paste(na.omit(c_across(starts_with("study_school_type_"))), collapse = "; "),
         study_state = paste(na.omit(c_across(starts_with("study_state_"))), collapse = "; "),
         study_school_level = ifelse(study_school_level == "High School; Only reported Secondary School", "High School", study_school_level),) %>%
  mutate(across(where(is.character), ~str_replace_all(., "; Cannot tell", ""))) %>%
  ungroup() %>%
  mutate(across(c(study_cluster_size, starts_with("study_percent"), starts_with("study_number"),
                  study_age_average, study_publication_year), ~ as.numeric(replace(., . == -999, NA))))

group_td <- group_df %>%
  distinct(across(-user), .keep_all = TRUE) %>%
  rowwise() %>%
  mutate(study_group_format = paste(na.omit(c_across(starts_with("study_group_format_"))), collapse = "; "),
         study_group_provider = paste(na.omit(c_across(starts_with("study_group_provider_"))), collapse = "; "),
         study_group_recipients = paste(na.omit(c_across(starts_with("study_group_recipients_"))), collapse = "; "),
         study_group_location = paste(na.omit(c_across(starts_with("study_group_location_"))), collapse = "; "),
         study_group_mode = paste(na.omit(c_across(starts_with("study_group_mode_"))), collapse = "; "),
         study_group_recipients = paste(na.omit(c_across(starts_with("study_group_recipients_"))), collapse = "; "),
         study_group_name = case_when(study_group_type == "Intervention" ~ study_intervention_name,
                                      study_group_comparison_type == "Active" ~ paste0("Active: ", study_comparison_name),
                                      TRUE ~ as.character(study_group_comparison_type))) %>%
  ungroup() %>%
  mutate(across(where(is.character), ~str_replace_all(., "; Cannot tell", "")),
         study_group_intensity = ifelse(str_detect(study_group_intensity, "-") & study_group_intensity != "-999",
                                        str_extract(study_group_intensity, "(?<=-).*"),
                                        study_group_intensity)) %>%
  mutate(across(c(study_group_duration, study_group_sessions, study_group_intensity), ~ifelse(. == "-999", NA, as.numeric(.))))

#create df of study names and refids for merging
study_idbyname <- study_td %>%
  select(refid, study_author_year)

#outcome tidying
outcome_td <- outcome_df %>%
  distinct(across(-user), .keep_all = TRUE)

# irob_td <- study_rob_df %>%
#   select(refid, starts_with("irob")) %>%
#   filter(rowSums(!is.na(select(., -refid))) > 0) %>%
#   select(refid, ends_with("rating"), ends_with("judgment"), ends_with("decision"))
#
# crob_td <- study_rob_df %>%
#   select(refid, starts_with("crob")) %>%
#   filter(rowSums(!is.na(select(., -refid))) > 0) %>%
#   select(refid, ends_with("rating"), ends_with("judgment"), ends_with("decision"))
#
# robins_td <- study_rob_df %>%
#   select(refid, starts_with("robins")) %>%
#   filter(rowSums(!is.na(select(., -refid))) > 0) %>%
#   select(refid, ends_with("rating"), ends_with("judgment"), ends_with("decision"))

# #review level
# review_td <- review_df %>%
#   select(-starts_with("amstar"), -starts_with("robis")) %>%
#   mutate(review_publication_year = as.numeric(review_publication_year),
#          review_author_year = ifelse(refid == 2734, "Hugh-Jones 2021",
#                                      review_author_year))
#
# amstar_rating_td <- review_df %>%
#   select(refid, review_author_year, starts_with("amstar")) %>%
#   select(refid, review_author_year, ends_with("rating"))
#
# robis_rating_td <- review_df %>%
#   select(refid, review_author_year, starts_with("robis")) %>%
#   select(refid, review_author_year, ends_with("decision"), robis_overall_a, robis_overall_b, robis_overall_c,
#          robis_overall_rating)

## Format #####################################################################
#select data for table
a5_info <- study_td %>%

  # select(refid, study_author_year, study_start_date, study_end_date, #recruitment_approach,
  #        study_eligibility_criteria, study_research_design, study_assignment_level,
  #        study_cluster_level, study_cluster_size, study_number_groups,
  #        starts_with("study_percent"), starts_with("study_number"), study_grade_level, study_age_average, study_age_dispersion,
  #        study_country, study_state, study_school_area, study_school_level, study_flow_diagram, study_registration, study_availability_statement, study_age_dispersion_type, study_age_avg_type) %>%

  mutate(study_school_level = str_remove_all(study_school_level, "Only reported"),
         study_school_level = str_replace(study_school_level, "  ", " "),
         study_school_level = str_trim(study_school_level, side = "left"),
         across(where(is.character), ~ str_replace(., "Cannot tell", "Not reported")),
         study_research_design = ifelse(study_research_design == "QED - Regression adjustment", "Quasi-experimental design", study_research_design)) %>%

  mutate(study_start_date = ifelse(nchar(study_start_date) == 10, format(ymd(study_start_date), "%B %d, %Y"), study_start_date),
         study_end_date = ifelse(nchar(study_end_date) == 10, format(ymd(study_end_date), "%B %d, %Y"), study_end_date)) %>%
  mutate(study_start_date = ifelse(nchar(study_start_date) == 7, format(ym(study_start_date), "%B %Y"), study_start_date),
         study_end_date = ifelse(nchar(study_end_date) == 7, format(ym(study_end_date), "%B %Y"), study_end_date)) %>%
  mutate(study_number_participants = format(study_number_participants, big.mark = ",", scientific = FALSE),
         sample_size = case_when(!is.na(study_number_participants) & !is.na(study_number_classrooms) & !is.na(study_number_schools) ~
                                   paste(study_number_participants, "students;", study_number_classrooms, "classrooms;", study_number_schools, "schools"),
                                 !is.na(study_number_participants) & !is.na(study_number_classrooms) & is.na(study_number_schools) ~
                                   paste(study_number_participants, "students;", study_number_classrooms, "classrooms"),
                                 !is.na(study_number_participants) & is.na(study_number_classrooms) & is.na(study_number_schools) ~
                                   paste(study_number_participants, "students"),
                                 !is.na(study_number_participants) & is.na(study_number_classrooms) & !is.na(study_number_schools) ~
                                   paste(study_number_participants, "students;", study_number_schools, "schools"),
                                 is.na(study_number_participants) & !is.na(study_number_classrooms) & !is.na(study_number_schools) ~
                                   paste(study_number_classrooms, "classrooms;", study_number_schools, "schools"),
                                 is.na(study_number_participants) & !is.na(study_number_classrooms) & is.na(study_number_schools) ~
                                   paste(study_number_classrooms, "classrooms"),
                                 is.na(study_number_participants) & is.na(study_number_classrooms) & !is.na(study_number_schools) ~
                                   paste(study_number_schools, "schools")),
         sample_size = str_trim(sample_size, side = "left"),
         grd_schl_level = paste(study_grade_level, "/", study_school_level),
         age_mean_sd = case_when(study_age_avg_type == "Mean" & study_age_dispersion_type == "Standard deviation" ~
                                   paste0("Mean = ", study_age_average, " (SD = ", study_age_dispersion, ")"),
                                 study_age_avg_type == "Mean" & study_age_dispersion_type == "Range" ~
                                   paste0("Mean = ", study_age_average, " (Range = ", study_age_dispersion, ")"),
                                 study_age_avg_type == "Median" & study_age_dispersion_type == "Range" ~
                                   paste0("Median = ", study_age_average, " (Range = ", study_age_dispersion, ")"),
                                 study_age_avg_type == "Not reported" & study_age_dispersion_type == "Range" ~
                                   paste0("Range = ", study_age_dispersion),                                 ),
         study_percent_female = ifelse(!is.na(study_percent_female), paste0(study_percent_female * 100, "%"), "Not reported"),
         study_percent_ell = ifelse(!is.na(study_percent_ell), paste0(study_percent_ell * 100, "%"), "Not reported"),
         study_percent_frpl = ifelse(!is.na(study_percent_frpl), paste0(study_percent_frpl * 100, "%"), "Not reported"),
         percent_race_ethnicity = case_when(is.na(study_percent_white) & is.na(study_percent_black) & is.na(study_percent_aian) &
                                              is.na(study_percent_asian) & is.na(study_percent_nhpi) & is.na(study_percent_hispanic) &
                                              is.na(study_percent_mixed) & is.na(study_percent_other) ~ "Not reported",
                                            TRUE ~ paste0(study_percent_aian * 100, "% AIAN, ", study_percent_asian * 100, "% Asian, ",
                                                          study_percent_black * 100, "% Black, ", study_percent_hispanic * 100, "% Latinx, ",
                                                          study_percent_nhpi * 100, "% NHPI, ", study_percent_white * 100, "% White, ",
                                                          study_percent_mixed * 100, "% Mixed, ", study_percent_other * 100, "% Other")),
         country_state = case_when(study_state != "" ~ paste0(study_country, " (", study_state, ")"),
                                   TRUE ~ study_country)) %>%
  mutate(age_mean_sd = case_when(age_mean_sd == "NA (-999)" ~ "Not reported",
                                 str_detect(age_mean_sd, "-999") ~ paste(study_age_average),
                                 TRUE ~ age_mean_sd),
         percent_race_ethnicity = str_replace_all(percent_race_ethnicity, c("NA% AIAN, " = "", "NA% Asian, " = "", "NA% Black, " = "",
                                                                            "NA% Latinx, " = "", "NA% NHPI, " = "", "NA% White, " = "",
                                                                            "NA% Mixed, " = "", "NA% Other" = ""))) %>%
  mutate(percent_race_ethnicity = gsub(", $", "", percent_race_ethnicity)) %>%
  mutate(across(everything(), ~ifelse(.x == -999 | .x == "Cannot tell" | .x == "Cannot Tell" | is.na(.x), "Not reported", .x)),
         across(c(study_cluster_level, study_cluster_size), ~ifelse(study_assignment_level != "Cluster", "NA", .x))) %>%
  select(refid, study_author_year, study_start_date, study_end_date,
         study_eligibility_criteria, study_research_design, study_assignment_level,
         study_cluster_level, study_cluster_size, study_number_groups,
         sample_size, grd_schl_level, age_mean_sd, study_percent_female,
         percent_race_ethnicity, study_percent_ell, study_percent_frpl,
         country_state, study_school_area, study_school_level, study_flow_diagram,
         study_registration, study_availability_statement,
         study_grade_level, study_school_level, study_country, study_state, study_school_type, study_publication_year, study_cor_author) #added specifically for dashboard

# Update 11/14/2024: add program links
#import program links
int_links <- import(here("data", "APO_app_intervention_links.xlsx"))

t4_group <- group_td %>%
  select(refid, group_id, study_group_type, study_group_name, study_group_type,  study_group_comparison_type) %>%
  left_join(study_idbyname) %>%
  mutate_all(list(~ifelse(. == -999, "Not reported", .))) %>%
  mutate_at(vars(study_group_type, study_group_comparison_type), list(~str_remove(., "^[0-9]+\\. "))) %>%
  select(refid, study_author_year, everything()) %>%
  arrange(study_author_year) %>% 
  left_join(int_links %>% select(refid, Intervention, website_link, clearinghouse_link), 
            by = c("refid", "study_group_name" = "Intervention"))

#summarize intervention groups
interventions <- t4_group %>%
  filter(study_group_type == "Intervention") %>%
  group_by(refid) %>%
  summarize(
    Intervention = paste(study_group_name, collapse = "; and "),
    website_links = paste(unique(website_link[!is.na(website_link)]), collapse = " |~| "),
    clearinghouse_links = paste(unique(clearinghouse_link[!is.na(clearinghouse_link)]), collapse = " |~| "),
    .groups = "drop")

#summarize comparisons
comparisons <- t4_group %>%
  filter(study_group_type == "Comparison") %>%
  group_by(refid) %>%
  summarize(
    Comparison = paste(unique(study_group_name), collapse = "; and "),
    .groups = "drop")

#merge group names and transform to wide
t4group_wide <- interventions %>%
  full_join(comparisons, by = "refid") %>%
  mutate_all(~ str_replace_all(., "Cannot tell", "Not reported"))


#merge with group/intervention names created for table 4
a5_groups <- t4group_wide %>%
  mutate(refid = as.numeric(refid)) %>%
  right_join(a5_info)



#merge with outcome data and combine outcomes for each study
a5_outcome <- outcome_td %>%
  distinct(refid, study_outcome_domain, .keep_all = TRUE) %>%
  group_by(refid) %>%
  summarize(outcome_list = paste(study_outcome_domain, collapse = "; ")) %>%
  ungroup() %>%
  mutate(outcome_list = sapply(lapply(str_split(outcome_list, "; "), sort), paste, collapse = "; ")) %>%
  select(refid, outcome_list) %>%
  right_join(a5_groups)

# #merge with rob data created in table 4
# a5_rob <- t4_allrob %>%
#   right_join(a5_outcome)

#import all citations
all_citations_df <- import(here("data", "APO_all_citations.csv")) %>%
  janitor::clean_names()

#import citation info
study_elig_citations <- import(here("data", "APO_study_eligibility_citations.xlsx")) %>%
  janitor::clean_names()

#merge linked reference df and transform for merging
linked_study_td <- linked_ref_df %>%
  filter(refid %in% inc_ps_wlinked) %>%
  select(refid, linked_refid) %>%
  left_join(study_elig_citations) %>%
  left_join(all_citations_df, by = c("linked_refid" = "refid")) %>%
  rename(main_citation = bibliography.x,
         linked_citation = bibliography.y) %>%
  select(refid, main_citation, starts_with("linked")) %>%
  pivot_longer(cols = c(main_citation, linked_citation),
               names_to = "citation_type",
               values_to = "citation",
               values_drop_na = TRUE) %>%
  mutate(citation_refid = case_when(citation_type == "main_citation" ~ as.character(refid),
                                    citation_type == "linked_citation" ~ as.character(linked_refid))) %>%
  select(-c(linked_refid, citation_type)) %>%
  filter(refid != citation_refid)

#format public links df to merge
# study_links <- public_links %>%
#   mutate(author_year = str_replace(study_author_year, "(\\d{4}).*", "\\1"),
#          merge_author = stri_trans_general(author_year, "Latin-ASCII"),
#          merge_author = str_to_lower(merge_author)) %>%
#   distinct(merge_author, .keep_all = TRUE) %>%
#   select(-study_author_year, -author_year, -refid)

#create variable that combines all references for a review into one cell
study_reports <- study_elig_citations %>%
  filter(refid %in% inc_ps_wlinked) %>%
  select(refid, bibliography) %>%
  rename(citation = bibliography) %>%
  mutate(citation_refid = "") %>%
  rbind(linked_study_td) %>%
  filter(citation_refid != 10487) %>%
  group_by(refid) %>%
  summarize(all_reports = paste(citation, collapse = " <br><br> ")) %>%
  mutate(all_reports = stringi::stri_trans_general(all_reports, "Latin-ASCII")) %>%
  ungroup()


#extract titles from citation df
study_titles <- study_elig_citations %>%
  select(refid, title)

#merge together
a5 <- a5_outcome %>%
  left_join(study_reports) %>%
  left_join(study_titles) %>%
  left_join(public_links) %>%
  mutate(study_eligibility_criteria = str_replace(study_eligibility_criteria, ". Exclude:", ". <br> Exclude:")) %>%
  select(study_author_year, study_publication_year, title, study_cor_author, #study_start_date, study_end_date,
         #study_eligibility_criteria, study_research_design, study_assignment_level, study_cluster_level,
         #study_cluster_size, study_number_groups,
         country_state, study_school_area, study_school_type,
         grd_schl_level, sample_size, #age_mean_sd,
         percent_race_ethnicity, study_percent_ell, study_percent_frpl, study_percent_female,
         Intervention, outcome_list,
         #study_flow_diagram, study_registration, study_availability_statement, all_reports,
         study_state, study_country, study_grade_level, study_school_level, starts_with("link"), ends_with("links")) %>% #removed variables from datatable re: translational team 10/4/24
  mutate_if(is.numeric, as.character) %>%
  arrange(str_to_lower(stringi::stri_trans_general(study_author_year, "Latin-ASCII")))

# Export cleaned data for use in app
# export(a5, here("outputs", "data_dashboard", "data", "apo_app_data.xlsx"))
# export(a5, here("data", "apo_app_data.xlsx"))