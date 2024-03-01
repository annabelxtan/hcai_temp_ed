#Descriptive Tables on Case Only Dataset 
#Dec 13 2023

rm(list=ls())

source("0_00_packages_to_load.R")
source("00_functions1.R")

library(tableone)

newpath_clean = "data_csv ed files updated clean/"
newpath_casecontrol = "data_case control/"

#run this to test if functions are working
# ed2013 = read_xlsx(here::here(paste0(newpath_clean, "ed2013.xlsx")))
# ed2020 = read_xlsx(here::here(paste0(newpath_clean, "ed2020.xlsx")))
# ed2013 = combinedfct(ed2013)
# ed2020 = combinedfct(ed2020)


#combines all ED data from years 2013 to 2020 
edfiles <- list.files(path = newpath_clean, pattern = "\\.xlsx$", full.names = TRUE)
dataframes_list <- lapply(edfiles, read_excel)
combined_df <- rbindlist(dataframes_list, use.names = TRUE)

combined_df = combinedfct(combined_df)

#creates unique IDs for every case 
df1 = create_unique_id(combined_df)
unique_ids <- unique(df1$unique_id)
unique_id_count <- length(unique_ids)
print(unique_id_count)


df2  = censusfct(df1)

saveRDS(df1, here::here(paste0(newpath_casecontrol, "case_uniqueID_12_21_23.rds")))
saveRDS(df2, here::here(paste0(newpath_casecontrol, "case_uniqueID_census_01_06_24.rds")))





