#Creating Case Control Dataset with Control and Lag days
#Then merge with PM2.5 and temp data (so that you can account for temps of control days)
#Updated Jan 16 2023

rm(list=ls())

source("0_00_packages_to_load.R")
source("00_functions1.R")

newpath_casecontrol = "data_case control/"
df1 = readRDS(here::here(paste0(newpath_casecontrol, "case_uniqueID_12_21_23.rds")))
              
#add control days
newdata = create_control_days_by_year(df1)

newdata = newdata %>%
  arrange(unique_id)

#add lag days to case and control days 
newdata1 = create_lag_days_by_year(newdata)


#merge by date_lag, not date_control for lag dataset
newdata1$date_lag <- as.Date(newdata1$date_lag, format = "%m/%d/%Y")

newdata1 = newdata1 %>%
  mutate(datenew = as.Date(date_lag),
         zip = as.integer(patzip))

#read in temp and pm2.5 data
merged_data_df = readRDS(here::here("temp_pm25only.RDS"))

bigmerge = left_join(newdata1, merged_data_df, by=c("datenew", "zip"))

bigmerge = bigmerge %>%
  select(-Year)

saveRDS(bigmerge, paste0(newpath_casecontrol,"casecontrol_lag_01_18_24.rds"))

# bigmerge = censusfct(bigmerge) #this part breaks down with lag days added in 

head(bigmerge)

saveRDS(bigmerge, paste0(newpath_casecontrol,"casecontrol_01_12_24.rds"))


#Summarize by cause
# summarise by date, zcta, count of hospitalizations

dat.complete.summary = bigmerge %>%
  group_by(datenew, zip) %>%
  summarise(freq=n()) %>%
  rename(hosps=freq)



# save entire list of causes and hide any values 10 or fewer
dat.complete.summary.more.10 = dat.complete.summary %>%
  mutate(hosps=as.numeric(hosps)) %>%
  filter(hosps > 10)
