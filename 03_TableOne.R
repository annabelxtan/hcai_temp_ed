#Creating Tables
#Jan 6 2023

source("0_00_packages_to_load.R")
source("00_functions1.R")

library(tableone)

newpath_casecontrol = "data_case control/"

df1 = readRDS(here::here(paste0(newpath_casecontrol, "case_uniqueID_census_01_06_24.rds")))

df1 = df1 %>%
  mutate(rural = factor(rural))

#data summary
data.summary.by.yr = df1 %>%
  group_by(condition, yearserv) %>%
  summarise(freq = n()) %>%
  group_by(yearserv) %>%
  mutate(percentage_by_year= (freq / sum(freq)) * 100) %>%
  arrange(yearserv)

#plots frequency of CV related hospitalizations 2013-2020
library(RColorBrewer)
plot = ggplot(data.summary.by.yr, aes(x = yearserv, y = freq, fill = condition)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Frequency of CV related hospitalizations 2013-2020",
       x = "Year",
       y = "Freq",
       fill="Condition") +
  theme_minimal() +
  scale_fill_brewer(palette = "Dark2", name = "Condition", labels = c("cerebrovascular disease", "dysrhythmia", "heart failure", "ischemic heart disease", "peripheral arterial disease", "pulmonary embolism")) 

plot

#creating table one
catVars = c('age', 'sexnew', 'ethnicity', 'racenew', 'ginicoeff', 'rural')
myVars = c('condition')
tab <- CreateTableOne(vars = catVars, strata = myVars, data = df1)
print(tab, quote = TRUE, noSpaces = TRUE)

hist(df1$agyrserv, 
     main = "Distribution of ages in HCAI data 2013-2020", 
     xlab = "Age at time of admission (years)")

