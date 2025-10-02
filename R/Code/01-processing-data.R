# Reproducible Research Fundamentals 
# 01. Data processing

install.packages("haven")
install.packages("dplyr")
install.packages("tidyr")
install.packages("stringr")
install.packages("labelled")

### Libraries
library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(labelled)

### Loading data ----

# Load the dataset
data_path <- "C:/Users/wb618037/OneDrive - WBG/Desktop/DataWork/DataWork/Data"
data      <- read_dta(file.path(data_path, "Raw/TZA_CCT_baseline.dta"))

### Remove duplicates based on hhid
data_clean <- data %>%  distinct(hhid, .keep_all = TRUE)



### Household (HH) level data ----

#### Tidying data for HH level
data_tidy_hh <- data_clean %>%
  select(
    vid,
    hhid,
    enid,
    floor:n_elder,
    food_cons:last_col()
  )

library(tidyr)
data_tidy_hh_member <- data_clean %>%
  select(
    vid, hhid, enid,
    starts_with("gender"),
    starts_with("age"),
    starts_with("read"),
    starts_with("clinic_visit"),
    starts_with("sick"),
    starts_with("days_sick"),
    starts_with("treat_fin"),
    starts_with("treat_cost"),
    starts_with("ill_impact"),
    starts_with("days_impact")
  ) %>%
  pivot_longer(
    cols = -c(vid, hhid, enid),
    names_to = c(".value", "member"),
    names_pattern = "(.*)_(\\d+)"
  )

library(stringr)
library(labelled)

data_clean_hh <- data_tidy_hh %>%
  mutate(submissiondate = as.Date(submissionday, format = "%Y-%m-%d %H:%M:%S")) %>%
  mutate(duration = as.numeric(duration)) %>%
  mutate(ar_unit = as.factor(ar_farm_unit)) %>%
  mutate(ar_unit = na_if(ar_unit, "")) %>%
  mutate(crop_other = str_to_title(crop_other)) %>%
  mutate(crop = case_when(
    str_detect(crop_other, "Coconut") ~ 40,
    str_detect(crop_other, "Sesame") ~ 41,
    TRUE ~ crop
  )) %>%
  mutate(across(where(is.numeric), ~ replace(., . == -88, NA))) %>%
  set_variable_labels(
    duration = "Duration of the interview (minutes)",
    submissiondate = "Submission date",
    ar_unit = "Farm area unit?"
  )


data_clean_hh_member <- data_tidy_hh_member %>%
  filter(!is.na(gender)) %>%
  set_variable_labels(
    member = "HH member ID",
    age = "Age",
    clinic_visit = "In the past 12 months, how many times has the member attended the clinic?",
    days_sick = "No. of days in the last 4 weeks the member suffered from the health problem?",
    treat_cost = "How much did the treatment cost?",
    days_impact = "No. of days member was unable to perform daily activities due to illness?"
  )



### Secondary data ----

# Load CSV data
secondary_data <- read.csv(file.path(data_path, "Raw/TZA_amenity.csv"))

# Tidying data
secondary_data <- secondary_data %>%
  pivot_wider(
    names_from = amenity,
    values_from = n,
    names_prefix = "n_"
  )


write_dta(data_clean_hh, file.path(data_path,"Intermediate/TZA_CCT_HH.dta"))
write_dta(data_clean_hh_member,file.path(data_path, "Intermediate/TZA_CCT_HH_mem.dta"))
write_dta(secondary_data, file.path(data_path,"Intermediate/TZA_amenity_tidy.dta"))


### Data cleaning for Household-member (HH-member) level
data_clean_hh <- data_tidy_hh %>%
    # Convert submissionday to date
    mutate(...... = as.Date(......, format = "%Y-%m-%d %H:%M:%S")) %>%
    # Convert duration to numeric (if it is not already)
    mutate(......) %>%
    # Convert ar_farm_unit to factor (categorical data)
    mutate(......) %>%
    # Replace values in the crop variable based on crop_other using regex for new crops
    mutate(crop = case_when(
        ......
    )) %>%
    # Recode negative numeric values (-88) as missing (NA)
    mutate(across(......)) %>%
    # Add variable labels
    set_variable_labels(
        ......
    )

# Save the household data
write_dta(data_clean_hh, file.path(data_path, "Intermediate/TZA_CCT_HH.dta"))

### Household member (HH-member) level data ----

#### Tidying data for HH-member level
data_tidy_mem <- data_dedup %>%
    select(......,
           starts_with(......)) %>%
    pivot_longer(cols = -c(vid, hhid, enid),  # Keep IDs static
                 names_to = ......,
                 names_pattern = "(.*)_(\\d+)")  # Capture the variable and the suffix

### Data cleaning for HH-member level
data_clean_mem <- data_tidy_mem %>%
    # Drop rows where gender is missing (NA)
    ...... %>%
    # Variable labels
    ......

# Save the tidy household-member data
write_dta(data_clean_mem, file.path(data_path, "Intermediate/TZA_CCT_HH_mem.dta"))

### Secondary data ----

# Load CSV data
secondary_data <- read.csv(file.path(data_path, "Raw/TZA_amenity.csv"))

# Tidying data
secondary_data <- secondary_data %>%
    pivot_wider(names_from = ......,
                values_from = ......,
                names_prefix = ......)

# Save the final tidy secondary data
write_dta(secondary_data, file.path(data_path, "Intermediate/TZA_amenity_tidy.dta"))
