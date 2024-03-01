#Adding lag days

rm(list=ls())

source("0_00_packages_to_load.R")
source("00_functions1.R")

newpath_casecontrol = "data_case control/"
df1 = readRDS(here::here(paste0(newpath_casecontrol, "casecontrol_01_12_24.rds")))

#DLNM is the lag part

