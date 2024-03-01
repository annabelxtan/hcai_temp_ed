####################################################################
# Functions for categorizing ICD9 codes
####################################################################

categorize_ICD9 = function(data) {
  
  conditions <- list(
    ischemic = "^410|^411|^413",
    pe = "^415",
    dys = "^426|^427",
    hf = "^428",
    pad = "^444", 
    cerebro = "^430|^431|^432|^433|^434|^435|^436|^437|^438"
  )
  
  data1 = data %>%
    mutate(
      condition = case_when(
        grepl(conditions$ischemic, dx_prin) ~ "ischemic",
        grepl(conditions$pe, dx_prin) ~ "pe",
        grepl(conditions$dys, dx_prin) ~ "dys",
        grepl(conditions$hf, dx_prin) ~ "hf",
        grepl(conditions$pad, dx_prin) ~ "pad",
        grepl(conditions$cerebro, dx_prin) ~ "cerebro",
        TRUE ~ "other" 
      ))
      
  return(data1)
}

####################################################################
# Function for categorizing ICD10 codes
####################################################################

categorize_ICD10 = function(data) {
  
  conditions <- list(
    ischemic = "^410|^411|^413|^I20|^I21|^I22|^I23|^I24|^I25",
    pe = "^415|^I26|^I27|^I28",
    dys = "^426|^427|^I44|^I45|^I46|^I47|^I48|^I49",
    hf = "^428|^I50",
    pad = "^444|^I73",
    cerebro = "^430|^431|^432|^433|^434|^435|^436|^437|^438|^G45|^G46|^I60|^I61|^I62|^I63|^I64|^I65|^I66|^I67|^I68|^I69"
  )
  
  data1 = data %>%
    mutate(
      condition = case_when(
        grepl(conditions$ischemic, dx_prin) ~ "ischemic",
        grepl(conditions$pe, dx_prin) ~ "pe",
        grepl(conditions$dys, dx_prin) ~ "dys",
        grepl(conditions$hf, dx_prin) ~ "hf",
        grepl(conditions$pad, dx_prin) ~ "pad",
        grepl(conditions$cerebro, dx_prin) ~ "cerebro",
        TRUE ~ "other" 
      )) %>%
    mutate(yearserv = format(as.Date(serv_dt, format = "%m/%d/%Y"), "%Y"))
  
  return(data1)
}



# ischemic = "^I20|^I21|^I22|^I23|^I24|^I25",
# pe = "^I26|^I27|^I28",
# dys = "^I44|^I45|^I46|^I47|^I48|^I49",
# hf = "^I50",
# pad = "^I73",

#################################################################################
# Function for creating dataset by year with census variables
#################################################################################

censusfct <- function(data) {
  folder_path <- "/Users/Annabel/Library/CloudStorage/Box-Box/Annabel Tan's Files/Proj2_HCAI/HCAI_proj/data_gini/"
  folder_path2 <- "/Users/Annabel/Library/CloudStorage/Box-Box/Annabel Tan's Files/Proj2_HCAI/HCAI_proj/data_urban/"
  
  ginifiles <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)
  years <- sub(".*_(\\d{4})\\.csv$", "\\1", basename(ginifiles))
  dataframes_list <- lapply(ginifiles, read.csv)
  dataframes_list_with_year <- Map(function(df, year) {
    df$year <- year
    return(df)
  }, dataframes_list, years)
  
  gini <- rbindlist(dataframes_list_with_year, use.names = TRUE)

  gini <- gini %>%
    rename(zip = GEOID) %>%
    rename(ginicoeff = estimate) %>%
    select(zip, ginicoeff, year)
    
  # data1 = data %>%
  #   rename(zip = patzip) %>%
  #   rename(year = yearserv)
  
  data1 = data

  df_merge <- merge(data1, gini, by = c("zip", "year"))
  
  urb_rural = read.csv(paste0(folder_path2, "urb_rural_binary.csv"))
  
  df_merge1 = merge(df_merge, urb_rural, by = c("zip"))
  
  return(df_merge1)
}


#################################################################################
# Function for creating dataset by year with pm2.5, ED data, temperature data
#################################################################################


merge_temp_pm <- function(year) {
  
  folder_path <- "/Users/Annabel/Library/CloudStorage/Box-Box/Annabel Tan's Files/Proj2_HCAI/HCAI_proj/data_csv ed files updated clean/"
  
  pmfolderpath <- "/Users/Annabel/Library/CloudStorage/Box-Box/Annabel Tan's Files/Proj2_HCAI/HCAI_proj/data_pm25_aguilera/"
  
  tempfolderpath <- "/Users/Annabel/Library/CloudStorage/Box-Box/Annabel Tan's Files/Proj2_HCAI/R code/prism/data/clean/"
  
  pm <- read.csv(paste0(pmfolderpath,paste0("CAzip_wfpm25_",year,".csv")))
  
  temp <- readRDS(paste0(tempfolderpath,paste0("prism_zip_day_ca_2013_2020.rds")))
 
  temp <- temp %>%
    filter(date >= paste0(year, "-01-01") & date <= paste0(year, "-12-31")) %>%
    mutate(datenew = date)
  
  pm <- pm %>%
    mutate(datenew = as.Date(date),
           zip = zip_code) %>%
    filter(year(datenew) == year) 
  
  df_merged <- merge(temp, pm, by = c("datenew", "zip"))
  
  df_merged <- subset(df_merged, select = -c(date.x, date.y))
  

  return(df_merged)
}


merge_data_by_year <- function(data) {
  # Read in your data based on the year
  
  data <- data %>%
    mutate(datenew=as.Date(date))
  
  # Perform operations on the ED data
  ed_processed <- ed %>%
    arrange(pat_id) %>%
    mutate(datenew = as.Date(serv_dt, format = "%m/%d/%y"),
           zip = patzip)
  
  # Merge the datasets
  df_merged <- merge(ed_processed, temp, by = c("datenew", "zip"))
  df_merged <- merge(df_merged, pm, by = c("datenew", "zip"))
  
  # df_merged <- subset(df_merged, select = -c(date.x, date.y))
  
  return(df_merged)
}

####################################################################
# Functions for categorizing demographic variables
####################################################################

categorize_demo = function(data) {
  data1 = data %>%
    mutate(age = ifelse(agyrserv < 45, "<45", 
                        ifelse(agyrserv >= 45 & agyrserv < 55, "45-54",
                               ifelse(agyrserv >= 55 & agyrserv < 64, "55-64",
                                      ifelse(agyrserv >= 65 & agyrserv < 75, "65-74", 
                                             ifelse(agyrserv >= 75 & agyrserv < 84, "75-84",
                                                    "85+")))))) %>%
    mutate(ethnicity = case_when(
        eth == "E1" ~ "Hispanic/Latino",         
        eth == "E2" ~ "Non-Hispanic/Non-Latino",
        eth == "99" ~ "Unknown",
        eth == "-" ~ "Invalid",
        TRUE ~ NA_character_
      )) %>%
    mutate(racenew = case_when(
        race == "R1" ~ "Am Indian/Alaska Native",
        race == "R2" ~ "Asian",
        race == "R3" ~ "Black/Afr Amer",
        race == "R4" ~ "Native Hawaiian/Pac Isl",
        race == "R5" ~ "White",
        race == "R9" ~ "Other",
        race == "99" ~ "Unknown",
        race == "-" ~ "Invalid",
        TRUE ~ NA_character_
      )) %>%
  mutate(race_group = case_when (
        race_grp == "1" ~ "White",
        race_grp == "2" ~ "Black",
        race_grp == "3" ~ "Hispanic",
        race_grp == "4" ~ "Asian",
        race_grp == "5" ~ "Am Indian/Alaska Native",
        race_grp == "6" ~ "Native Hawaiian/Pac Isl",
        race_grp == "7" ~ "Multiracial",
        race_grp == "8" ~ "Other",
        race_grp == "0" ~ "Unknown",
        race_grp == "-" ~ "Invalid",
        TRUE ~ NA_character_
  )) %>%
    mutate(sexnew = case_when (
      sex == "M" ~ "Male",
      sex == "F" ~ "Female",
      sex == "U" ~ "Unknown", 
      sex == "-" ~ "Invalid",
      TRUE ~ NA_character_
    ))
}


####################################################################
# Functions in a function (cat ICD 9 and demo)
####################################################################

combinedfct = function(data) {
  data1 = categorize_ICD10(data)
  data1 = categorize_demo(data1)
  
  data1 = data1 %>%
    rename(date = serv_dt) %>%
    mutate()
  
  return(data1)
}

###################################################################################################
# Create unique id for each admission based on patid, patzip, faczip and admission date
###################################################################################################

library(digest)

create_unique_id <- function(data) {
  # Concatenating the columns to create a unique string
  data$unique_id <- apply(data, 1, function(row) {
    paste(row["pat_id"], row["pat_zip"], row["faczip"], as.character(row["date"]), sep = "_")
  })
  
  # Hashing the combined string to create a unique ID
  data$unique_id <- sapply(data$unique_id, digest)
  
  return(data)
}


####################################################################
# Functions for creating control days
####################################################################

# function create control days from case day
make_control_day <- function(data, BeforeAfter, WK){ 
  
  data$date <- as.Date(data$date, format = "%m/%d/%Y")
  
  # The name of the day; accounts for if control or case
  VarName <- paste0(BeforeAfter, '_', str_trunc(WK, 1, 'left', ''))
  class(data$date)
  # adds WKs number of weeks
  data = data %>% mutate(!!VarName := date + lubridate::weeks(WK))
  
}

# function to create control days
create_control_days_by_year <- function(df.year){
  
  year_current = unique(df.year$year)
  print(paste0('processing case control for ', year_current))
  print(paste0('Total records: ', nrow(df.year)))
  
  # use function above to create bidirectionally symmetric dates 
  days1 <- df.year
  days1 <- make_control_day(days1, 'Before', -4)
  days1 <- make_control_day(days1, 'Before', -3)
  days1 <- make_control_day(days1, 'Before', -2)
  days1 <- make_control_day(days1, 'Before', -1)
  days1 <- make_control_day(days1, 'CaseDay', 0)
  days1 <- make_control_day(days1, 'After', 1)
  days1 <- make_control_day(days1, 'After', 2)
  days1 <- make_control_day(days1, 'After', 3)
  days1 <- make_control_day(days1, 'After', 4)
  
  # put in long format by Day
  days2 <- days1 %>% 
    gather('DayName', 'date_control', contains('CaseDay_'),
           contains('Before_'), contains('After_') ) 
  
  # stratify by month of event 
  days3 <- days2 %>% filter(month(date) == month(date_control))
  
  # case/control marker
  days3 <- days3 %>%
    mutate(ck = if_else(DayName=='CaseDay_0', 1, 0))
  
  print(paste0('Processed records: ', nrow(days3 %>% dplyr::filter(ck==1))))
  
  return(days3)
  # 
  # # output results
  # casecontrol.cause.folder = paste0(casecontrol.folder,cause,'/')
  # ifelse(!dir.exists(casecontrol.cause.folder), dir.create(casecontrol.cause.folder), FALSE)
  # saveRDS(days3, paste0(casecontrol.cause.folder,cause,'_case_control_',year_current,'.rds'))
}

####################################################################
# Functions for creating lag days 
####################################################################

# create lag days from case/control day
make_lag_day <- function(data, lag){
  # adds lag to date of interest
  data = data %>% mutate(!!lag := date_control + as.period(as.numeric(lag), 'day'))  
}

# function to create lag days
create_lag_days_by_year <- function(df.year){
  
  year_current = unique(df.year$year)
  print(paste0('processing lag days for ', year_current))
  
  # use function above to create bidirectionally symmetric dates 
  days1 <- df.year
  days1 <- make_lag_day(days1, '-6')
  days1 <- make_lag_day(days1, '-5')
  days1 <- make_lag_day(days1, '-4')
  days1 <- make_lag_day(days1, '-3')
  days1 <- make_lag_day(days1, '-2')
  days1 <- make_lag_day(days1, '-1')
  days1 <- make_lag_day(days1, '0')
  
  # put in long format by Day
  days2 <- days1 %>% 
    gather('lag', 'date_lag', c(`-6`,`-5`,`-4`,`-3`,`-2`,`-1`,`0`)) %>%
    mutate(lag=as.numeric(lag)) %>%
    arrange(unique_id,desc(ck),desc(lag))
  
  return(days2)
  
  # # output results
  # casecontrol.cause.lag.folder = paste0(casecontrol.lag.folder,cause,'/')
  # ifelse(!dir.exists(casecontrol.cause.lag.folder), dir.create(casecontrol.cause.lag.folder,recursive=TRUE), FALSE)
  # saveRDS(days2, paste0(casecontrol.cause.lag.folder,cause,'_case_control_lag_',year_current,'.rds'))
}

####################################################################
# DLNM-SPECIFIC FUNCTIONS
####################################################################

crossbasis_form = function(exp,df_var,df_lag){
  cb.temp <- crossbasis(exp,
                        lag=c(0,6),
                        argvar=list(fun = "ns", df = df_var),
                        arglag=list(fun = "ns", df = df_lag))
}

#### FREQUENTIST PROCESSING AND PLOTTING DLNM ####

plot_dlnm_master = function(cb,mod){
  pred =  pred_nonlin(cb,mod)
  plot_3d_dlnm(pred)
  plot_lags_dlnm(pred,0,1)
  plot_lags_dlnm(pred,2,3)
  plot_lags_dlnm(pred,4,6)
  plot_cumulative_dlnm(pred)
}

# predict from dnlm model
pred_nonlin <- function(cb, mod,temp=temp_mean){
  result = crosspred(cb,mod,cen=temp,by=1) 
}

# 3-D plot of dlnm
plot_3d_dlnm <- function(pred){
  plot(pred, zlab="OR", xlab="Temperature (C)", ylab="Lag (days)",
       main=expression("3D graph of temperature association"),
       theta=40, phi=30, lphi=30)
}

# Plot exposure-response for all lags
plot_lags_dlnm = function(pred,lag_start,lag_end){
  plot(pred, "slices", lag=c(lag_start:lag_end), col=2, ci.arg=list(density=40,col=grey(0.7)))
}

# Plot cumulative association
plot_cumulative_dlnm = function(pred){
  name = deparse(substitute(pred))
  plot(pred, "overall", col=2, xlab = "Temperature (C)", ylab="Cumulative OR",
       main=paste0("Overall cumulative association: ",name))
}

# Plot cumulative association as part of several to compare
plot_cumulative_dlnm_multi = function(cb,mod,temp=temp_mean,xlab="Temperature (C)",add=0,col_sel=1){
  pred =  pred_nonlin(cb,mod,temp)
  name = deparse(substitute(mod))
  if(add==0){
    plot(pred, "overall", col=2, xlab = xlab, ylab="Cumulative OR",
         main=name)
  }
  if(add==1){
    lines(pred, "overall", col=col_sel)
  }
  
}
