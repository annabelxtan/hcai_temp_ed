#january 19 2024
#break lag file down by year to make piecemeal analyses since my computer is breaking down 
rm(list=ls())

# 1a Declare root directory, folder locations and load essential stuff
project.folder = paste0(print(here::here()),'/')
source("0_00_packages_to_load.R")
source("00_functions1.R")
newpath_casecontrol = "data_case control/"
newpath_casecontrol_lag = "data_casecontrol_lag/"

#read in lag dataframe
df1 = readRDS(here::here(paste0(newpath_casecontrol, "casecontrol_lag_01_18_24.rds")))

# Set up a matching variable 
nr <- match(df1$unique_id, unique(df1$unique_id))

# Add the matching variable to the data frame
df1$nr <- nr


#split by year to make it easier to access
individual_dfs <- lapply(split(df1, df1$year), function(df_by_year) {
  # Perform any operations on the individual dataframes if needed
  return(df_by_year)
})

#saving one year at a home because i dont have computational power
# df_2013 <- individual_dfs$`2013`
# saveRDS(df_2013, "df_casecontrol_lag_2013.RDS")

df_2020 <- individual_dfs$`2020`
saveRDS(df_2020, paste0(newpath_casecontrol_lag, "df_casecontrol_lag_2020.RDS"))

#NOTE TO SELF ON JAN 24 2024
#need to save the rest
