#Merging PM2.5 and Temp with Cases only 
#Jan 6 2023

rm(list=ls())

source("0_00_packages_to_load.R")
source("00_functions1.R")

newpath_casecontrol = "data_case control/"

#merging pm2.5 and temp datasets
merged_data_list <- list()

for (year in 2013:2020) {
  merged_data <- merge_temp_pm(year) 
  merged_data_list[[as.character(year)]] <- merged_data
}

merged_data_df <- bind_rows(merged_data_list, .id = "Year")

df1 = readRDS(here::here(paste0(newpath_casecontrol, "case_uniqueID_census_01_06_24.rds")))

df1$date <- as.Date(df1$date, format = "%m/%d/%Y")

df1 = df1 %>%
  mutate(datenew = as.Date(date))

bigmerge = merge(df1, merged_data_df, by=c("datenew", "zip"))

bigmerge = bigmerge %>%
  select(-year.x, -year.y)

saveRDS(bigmerge, here::here(paste0(newpath_casecontrol, "case_uniqueID_census_temp_01_06_24.rds")))





