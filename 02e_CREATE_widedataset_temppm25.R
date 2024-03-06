#TURN DATA WIDE -- part 2
#PURPOSE: this code creates a large wide dataset with lag temps and lag pm2.5 values
#updated: march 3 2024
###########################################
rm(list=ls())

here::i_am("02e_CREATE_widedataset_temppm25.R")

# 1a Declare root directory, folder locations and load essential stuff
path1 = "data_casecontrol_lag_wide/"
functions.folder = "functions.folder/"
source("0_00_packages_to_load.R")
source("00_functions1.R")
# source(here::here(paste0(functions.folder, "00_frequentist_models_and_functions.R")))


#load data
df1 = readRDS(here::here(paste0(path1, "wide_control_lagtemp_COMPLETE.rds")))
df2 = readRDS(here::here(paste0(path1, "wide_control_lagpm25_COMPLETE.rds")))

head(df1)
head(df2)

df3 = merge(df1, df2, by = c("date", "date_control", "unique_id", "pat_id", "zip", "faczip", "patzip", "dx_prin", "DayName", "nr", "ck"))

unique_values <- unique(df1$unique_id)
num_unique_values <- length(unique_values)


df2[[unique_id]] <- as.character(df2[[unique_id]])

# Get the matching values
matching_values <- df1[[variable_to_check]][df1[[variable_to_check]] %in% df2[[variable_to_check]]]

# Get the matching values
matching_values <- df1[[unique_id]][df1[[unique_id]] %in% df2[[unique_id]]]

# Number of matching values
num_matching_values <- length(matching_values)


