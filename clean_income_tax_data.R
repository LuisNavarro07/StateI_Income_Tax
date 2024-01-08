#-------------------------------------------------------------------------------------------------------
# Project: Trends in state personal income taxes (PIT) from 2002-2023: Tax Decreasers, Increasers, Switchers
# Script: Clean Data for the Analysis
# Update: January 2024
# Code by: Luis Navarro 
#-------------------------------------------------------------------------------------------------------
rm(list = ls())
### Setup 
if(!require(pacman)) {install.packages("pacman")}
if(!require(tidyverse)) {install.packages("tidyverse")}
if(!require(tidycensus)) {install.packages("tidycensus")}
if(!require(tigris)) {install.packages("tigris")}
if(!require(grDevices)) {install.packages("grDevices")}
library(pacman)
pacman::p_load(tidyverse,
               tidycensus, tigris)
options(scipen = 999)
path = "C:/Users/luise/OneDrive - Indiana University/Research/State_Income_Tax_Project/"
bi = paste(path,"build/input/", sep ="")
bc = paste(path,"build/code/", sep ="")
bo = paste(path,"build/output/", sep ="")
bt = paste(path,"build/temp/", sep ="")
ai = paste(path,"analysis/input/", sep ="")
ac = paste(path,"analysis/code/", sep ="")
ao = paste(path,"analysis/output/", sep ="")
at = paste(path,"analysis/temp/", sep ="")

# FIPS Codes
fips_codes <- tigris::fips_codes %>% 
  group_by(state_name) %>% 
  summarize(state_fips = first(state_code)) %>% 
  mutate(state_fips = as.double(state_fips)) %>% 
  rename(state = state_name)

# Old Data 
tax_data_old <- read_csv(file = paste(bi, "CLEAN_TAX_DATA2.csv", sep = "")) %>% 
  select(State, year, High) %>% 
  rename(state = State) %>% 
  rename(value_old = High) %>% 
  mutate_at(vars(year), as.double) %>% 
  left_join(fips_codes, by = "state", relationship ="many-to-one")


# Updated Data for the Tax Notes Paper: 
# Source: Tax Foundation Data 
tax_data_new <- read_csv(file = paste(bi, "120323_state_pit_rates.csv", sep = "")) %>% 
  gather(key = "year", value = "value_new", -State) %>% 
  rename(state = State) %>% 
  mutate_at(vars(year), as.double) %>% 
  left_join(fips_codes, by = "state", relationship ="many-to-one")

# Merge Data 
tax_data <- tax_data_old %>% 
  left_join(tax_data_new, by = c("state", "year", "state_fips"), 
            relationship = "one-to-one") %>% 
  arrange(state, year) %>% 
  mutate(diff = value_new - value_old) %>% 
  mutate(diff_dum = ifelse(diff == 0, TRUE, FALSE )) %>% 
  mutate(taxrate = ifelse(is.na(value_new) == TRUE, value_old, value_new)) %>% 
  mutate(taxrate = case_when(
    state == "Vermont" & year == 2002 ~ 9.50, 
    state == "Vermont" & year == 2005 ~ 9.50,
    state == "Vermont" & year == 2006 ~ 9.50,
    state == "Utah" & year == 2023 ~ 4.65,
    TRUE ~ taxrate
  )) 
# Missing Values Vermont: 2002, 2005, and 2006.   
  
# Source: https://www.tax-brackets.org/vermonttaxtable/2002
  
  
# -----------------------------------------------------------------------------
# Categorization of States According to their Tax Policy Type  
tax_data_analysis <- tax_data %>% 
  select(state, year, taxrate, state_fips) %>% 
  arrange(state, year) %>% 
  relocate(state_fips, state, year) %>% 
  group_by(state) %>% 
  # Compute the Change in the Tax Rate by year 
  mutate(delta_tax = taxrate - lag(taxrate, default = taxrate[1])) %>% 
  # Cumulative Tax Change 
  mutate(cumulative_delta_tax = cumsum(delta_tax)) %>% 
  # Average Tax Change 
  mutate(mean_delta_tax = mean(delta_tax)) %>% 
  mutate(cumulative_taxrate = cumsum(taxrate)) %>% 
  mutate(cumulative_taxrate = mean(cumulative_taxrate)) %>% 
  mutate(year = as.integer(year)) %>% 
  mutate(taxdum = ifelse(delta_tax != 0, 1, 0), 
         taxinc = ifelse(delta_tax > 0, 1, 0), 
         taxdec = ifelse(delta_tax < 0, 1, 0)) %>%
  mutate(delta_tax_inc = delta_tax*taxinc, 
         delta_tax_dec = delta_tax*taxdec) %>% 
  arrange(state, year) %>%
  mutate(lastdeltatax = last(cumulative_delta_tax)) %>% 
  ## Assign categories
  mutate(tax_policy = case_when(
    mean_delta_tax == 0 & cumulative_taxrate == 0 ~ "No PIT Rate", 
    mean_delta_tax == 0 & cumulative_taxrate != 0 ~ "No Tax Change"
  )) %>% 
  mutate(tax_policy = case_when(
    is.na(tax_policy) == FALSE ~ tax_policy, 
    is.na(tax_policy) == TRUE & all(delta_tax >= 0) ~ "Tax Increasers",
    is.na(tax_policy) == TRUE & all(delta_tax <= 0) ~ "Tax Decreasers")) %>% 
  mutate(tax_policy = ifelse(is.na(tax_policy) == TRUE, "Tax Switchers", tax_policy)) %>% 
  mutate(tax_policy_seg = case_when(
    tax_policy != "Tax Switchers" ~ tax_policy, 
    tax_policy == "Tax Switchers" & lastdeltatax < 0 ~ "Tax Switchers - Net Decreaser", 
    tax_policy == "Tax Switchers" & lastdeltatax > 0 ~ "Tax Switchers - Net Increaser"
  )) %>% 
  select(state, year, taxrate, delta_tax, tax_policy, tax_policy_seg)

# Export to CSV file 
write_csv(x = tax_data_analysis, 
          file = paste(ai, "PIT_Rate_clean.csv", sep = ""))

# End Script 
#------------------------------------------------------------------------------
#-----------------------------------------------------------------------------
