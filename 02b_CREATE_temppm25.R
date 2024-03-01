#merging pm2.5 and temp datasets
merged_data_list <- list()

for (year in 2013:2020) {
  merged_data <- merge_temp_pm(year) 
  merged_data_list[[as.character(year)]] <- merged_data
}

merged_data_df <- bind_rows(merged_data_list, .id = "Year")

saveRDS(merged_data_df, "temp_pm25only.RDS")