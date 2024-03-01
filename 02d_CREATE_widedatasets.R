#turning dataset wide year by year
#jan 31 2024

rm(list=ls())

# 1a Declare root directory, folder locations and load essential stuff
path1 = "data_casecontrol_lag/"
path2 = "data_casecontrol_lag_wide/"
functions.folder = "functions.folder/"
source("0_00_packages_to_load.R")
source("00_functions1.R")
# source(here::here(paste0(functions.folder, "00_frequentist_models_and_functions.R")))

# Load case crossover lagged data (takes a while to load, 1GB file), need to load one at a time on my computer (change each year from 2013-2020)
df1 = readRDS(here::here(paste0(path1, "df_casecontrol_lag_2020.rds")))

dat.complete = df1 %>%
  mutate(lag=str_replace(lag, "-","lag")) %>%
  mutate(lag=str_replace(lag,"0","lag0")) 
# 
# %>%
#   mutate(tavg = (tmx + tmin)/2) 

#WIDE DATASET WITHOUT pm25, wfpm25, tmx
# Make into wide table with lags wide (takes a while) -- need to get rid of tdmn and tmin, and then rejoin later
# data_wide = dat.complete %>%
#   select(-date_lag, -datenew, -tdmn, -tmin, -pm25, -wf_pm25, -tmx) %>%
#   spread(lag, tavg)
# 
# head(data_wide)
# 
# saveRDS(data_wide, paste0(path2, "wide_control_lag_2020.RDS"))

#WIDE DATASET WITH pm25
data_wide_pm25 = dat.complete %>%
  select(-date_lag, -datenew, -tdmn, -tmin, -wf_pm25, -tmx) %>%
  spread(lag, pm25) %>%
  rename_with(~ paste0("lag_pm25_", str_remove(., "lag")), starts_with("lag")) %>%
  select(date, date_control, unique_id, zip, lag_pm25_0, lag_pm25_1, lag_pm25_2, lag_pm25_3, lag_pm25_4, lag_pm25_5, lag_pm25_6)

head(data_wide_pm25)

saveRDS(data_wide_pm25, paste0(path2, "wide_control_lagpm25_2020.RDS"))


#MERGE ALL THE WIDE FILES BACK
#feb 1 2024

# Set the path to your folder
folder_path <- "data_casecontrol_lag_wide/"

# file_list <- list.files(path = folder_path, full.names = TRUE)
rds_files <- list.files(folder_path, pattern = "\\.RDS$", full.names = TRUE)

# Define a function to read and load a file
load_file <- function(file_path) {
  data <- readRDS(here::here(paste0(file_path)))
  return(data)
}


loaded_data <- lapply(rds_files, load_file)

#combines all loaded files together
combined_data <- do.call(rbind, loaded_data)  # Assuming all files have the same structure (e.g., columns)

saveRDS(combined_data, paste0(folder_path, "wide_control_lagpm25_COMPLETE.RDS"))

