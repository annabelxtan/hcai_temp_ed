#Creating Cases only Dataset with Sociodemographic Data
#Jan 6 2023

rm(list=ls())

source("0_00_packages_to_load.R")
source("00_functions1.R")

newpath_casecontrol = "data_case control/"
df1 = readRDS(here::here(paste0(newpath_casecontrol, "case_uniqueID_12_21_23.rds")))

census_merge = censusfct(df1)

saveRDS(census_merge, here::here(paste0(newpath_casecontrol, "case_uniqueID_census_01_06_24.rds")))
