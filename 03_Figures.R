#Creating figures
#Jan 6 2024

rm(list=ls())
source("0_00_packages_to_load.R")
source("00_functions1.R")
library(scales)

newpath_casecontrol = "data_case control/"
newpath_population = "data_population/"

df1 = readRDS(here::here(paste0(newpath_casecontrol, "case_uniqueID_census_temp_01_06_24.rds")))

# Figure 1: plot emergency room visits on map, with color
#Maps -- plot emergency room visits on map
#using tmaps
#Code adapted from http://zevross.com/blog/2018/10/02/creating-beautiful-demographic-maps-in-r-with-the-tidycensus-and-tmap-packages/
#
# census_api_key("43433014cdd6694b4179f307c1b0361e4b21a382", overwrite = FALSE, install = FALSE)
# 
gini_county =  get_acs("zcta", table = "B19083", year = 2017,
                       output = "tidy", state = "CA", geometry = TRUE, cache_table = TRUE) %>%
  select(-moe)

gini_county = gini_county %>%
  rename(zip = GEOID)

df1_sub = df1 %>%
  mutate(tmax = signif(tmx, digits=2)) %>%
  select(date, Year, zip, dx_prin, tmax, condition) %>%
  mutate(zip=as.character(zip))
# 
df1_sub_condition = df1_sub %>%
  group_by(condition, zip, Year, tmax) %>%
  summarise(condition_total = n()) 



pop_data = read.csv(here::here(paste0(newpath_population, "population_data_2020.csv")))
pop_data = pop_data %>%
  select(-X, -NAME, -variable) %>%
  rename(zip = GEOID,
         pop = estimate) %>%
  select(-moe)


df1_sub_condition_tab = df1_sub_condition %>%
  merge(pop_data, by="zip") %>%
  mutate(rate = (condition_total/pop)) %>%
  filter(is.finite(rate)) %>%
  group_by(condition, tmax) %>%
  summarise(RatePer100k = sum(rate)*100000)


saveRDS(df1_sub_condition_tab, "df1sub_condition_02_05_24.RDS")

# df1_sub_tab = df1_sub %>%
#   group_by(zip) %>%
#   summarise(ERvisit=n())

# #Merge county shape files with subsetted data
# countymap <- left_join(df1_sub_tab, gini_county, by=c('zip'))
# 
# #turns dataframe into shape file
# zipmap = st_as_sf(countymap)
# 
# 
# saveRDS(zipmap, "zipmap_freq.RDS")


####LOAD ZIP MAP FREQ DATASET HERE ###########

zipmap = readRDS(here::here("zipmap_freq.RDS"))

zipmap = zipmap %>% mutate(zip=as.integer(zip))

#merge with population data to create per 100,000 plots 
pop_data = read.csv(here::here(paste0(newpath_population, "population_data_2020.csv")))

pop_data = pop_data %>%
  select(-X, -NAME, -variable) %>%
  rename(zip = GEOID,
         pop = estimate) 

#merge with pop data to find rates
zipmap1 = zipmap %>%
  merge(pop_data, by = "zip") %>%
  rename(gini = estimate) %>%
  mutate(pop = as.integer(pop)) %>%
  mutate(ERrate = (ERvisit/pop)*100000) %>%
  filter(is.finite(ERrate))

#filter out extreme values
percent_threshold <- 80
threshold_value <- quantile(zipmap1$ERrate, probs = percent_threshold / 100)

zipmap2 <- zipmap1 %>%
  mutate(ERrate1 = if_else(ERrate >= threshold_value, 0, ERrate)) 

#merge with temp data

newpath_temp = "data_temp_pm25/"
temp_pm25 = readRDS(here::here(paste0(newpath_temp, "temp_pm25only.RDS")))

temp = temp_pm25 %>%
  mutate(tmean = (tmx + tmin)/2)%>%
  group_by(Year, zip) %>%
  summarise(tmean_mean = mean(tmean)) 

temp1 = temp %>%
  group_by(zip) %>%
  summarise(tmean_mean_mean = mean(tmean_mean))


breaks_sequence <- seq(min(tempER$avg_temp_f), max(tempER$avg_temp_f), length.out = 6)  

tempER$quantiles <- cut(tempER$avg_temp_f, breaks = breaks_sequence, labels = FALSE)


tempER = zipmap2 %>%
  left_join(temp1, by = "zip") %>%
  rename(avg_temp = tmean_mean_mean) %>%
  mutate(avg_temp_f = (9/5)*avg_temp + 32) %>%
  drop_na(avg_temp_f) %>%
  mutate(templabel = case_when(
    avg_temp_f >= 43.0 & avg_temp_f < 49.9 ~ "43.0 - 49.8",
    avg_temp_f >= 49.9 & avg_temp_f < 56.6 ~ "49.9 - 56.5",
    avg_temp_f >= 56.6 & avg_temp_f < 63.2 ~ "56.6 - 63.2",
    avg_temp_f >= 63.3 & avg_temp_f < 69.9 ~ "63.3 - 69.8",
    avg_temp_f >= 69.9 & avg_temp_f < 78.0 ~ "69.9 - 78.0")) %>%
  drop_na(templabel)
  

#USE TEMP ER FOR BART CHARTS

#Now we have a dataset with all the geographic locations and gini coeffs
createMap1 <- function(.data, varname, maptitle){
  tm_shape(.data, projection = 2163, unit = "mi") +
    tm_polygons(varname,
                n = 8,
                # style = "cont",
                palette = RColorBrewer::brewer.pal(8, "YlOrRd"),
                border.col = "black",
                border.alpha = 0.1,
                title = "ER visits") +
    tm_legend(legend.position = c("left", "bottom")) +
    tm_layout(title = maptitle,
              title.size = 0.9,
              title.position = c("center", "top"),
              inner.margins = c(0.06, 0.10, 0.10, 0.08),
              frame = FALSE)
}

m1 <- createMap1(zipmap1,
                 varname = "ERvisit",
                 maptitle = "CV related ER Visits 2013-2020")
m1

#rates

createMap2 <- function(.data, varname, maptitle){
  tm_shape(.data, projection = 2163, unit = "mi") +
    tm_polygons(varname,
                n = 8,
                # style = "cont",
                palette = RColorBrewer::brewer.pal(8, "YlOrRd"),
                border.col = "#000000",
                border.alpha = 0.1,
                title = "ER visits per 100,000") +
    tm_legend(legend.position = c("left", "bottom")) +
    tm_layout(title = maptitle,
              title.size = 0.9,
              title.position = c("center", "top"),
              inner.margins = c(0.06, 0.10, 0.10, 0.08),
              frame = FALSE)
}

m2 <- createMap2(zipmap2,
                 varname = "ERrate1",
                 maptitle = "CV related ER Visits per 100,000 2013-2020")
m2


createMap3 <- function(.data, varname, maptitle){
  tm_shape(.data, projection = 2163, unit = "mi") +
    tm_polygons(varname,
                n = 10,
                # style = "cont",
                palette = RColorBrewer::brewer.pal(8, "YlOrRd"),
                border.col = "#000000",
                border.alpha = 0.1,
                title = "Gini coeff") +
    tm_legend(legend.position = c("left", "bottom")) +
    tm_layout(title = maptitle,
              title.size = 0.9,
              title.position = c("center", "top"),
              inner.margins = c(0.06, 0.10, 0.10, 0.08),
              frame = FALSE)
}

m3 <- createMap3(zipmap2,
                 varname = "gini",
                 maptitle = "Gini coefficient")
m3


#Figure 2: CV related conditions (absolute freq) 
df1 = readRDS(here::here(paste0(newpath_casecontrol, "case_uniqueID_census_01_06_24.rds")))

df1 = df1 %>%
  mutate(rural = factor(rural))

#data summary
data.summary.by.yr = df1 %>%
  group_by(condition, year) %>%
  summarise(freq = n()) %>%
  group_by(year) %>%
  mutate(percentage_by_year= (freq / sum(freq)) * 100) %>%
  arrange(year)

data.summary.by.yr.zip = df1 %>%
  group_by(condition, year, zip) %>%
  summarise(freq = n()) %>%
  group_by(year) %>%
  mutate(percentage_by_year= (freq / sum(freq)) * 100) %>%
  arrange(year)

data.summary.by.year.zip.pop = data.summary.by.yr.zip %>%
  left_join(pop_data, by=c("zip"))

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

#plot % instead of absolute number

#Figure 2a: CV related conditions (rates) 
plot1 = ggplot(data.summary.by.yr, aes(x = year, y = percentage_by_year, fill = condition)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Frequency of CV related hospitalizations 2013-2020",
       x = "Year",
       y = "% of total number of cases by year",
       fill="Condition") +
  theme_minimal() +
  scale_fill_brewer(palette = "Dark2", name = "Condition", labels = c("cerebrovascular disease", "dysrhythmia", "heart failure", "ischemic heart disease", "peripheral arterial disease", "pulmonary embolism")) 

plot1

# Figure 3: No of CV related ED visits (y) vs temp (x) by year (facet_wrap)
# Figure 3a - no facet wrap
df1 = readRDS(here::here(paste0(newpath_casecontrol, "case_uniqueID_census_temp_01_06_24.rds")))


df1_sub = df1 %>%
  mutate(tmax = signif(tmx, digits=2),
         tmin1 = signif(tmin, digits=2),
         zip=as.character(zip)) %>%
  select(date,Year, zip, dx_prin, tmax,tmin1,condition) %>%
  mutate(tmax_f = (9/5)*tmax + 32,
         tmin_f = (9/5)*tmin1 + 32) %>%
  group_by(Year, tmax_f, tmin_f, condition) %>%
  summarise(freq = n()) %>%
  ungroup()

df1_sub1 =  df1 %>%
  mutate(tmax = signif(tmx, digits=2),
         tmin1 = signif(tmin, digits=2),
         zip=as.integer(zip)) %>%
  select(date,Year, zip, dx_prin, tmax,tmin1,condition) %>%
  mutate(tmax_f = (9/5)*tmax + 32) %>%
  ungroup()


plot_a = ggplot(data=df1_sub1, aes(x=tmax_f,y=freq,  fill = tmax_f)) + 
  geom_bar(width = 0.9, stat = "identity") +
  labs(title = "Total Cardiovascular related ER visits 2013-2020",
       x = "TMax (F)",
       y = "Freq") + 
  scale_y_continuous(labels = comma) +
  scale_fill_viridis_c(option = "magma") + 
  theme_bw() +
  guides(fill = "none")  

plot_a

plot_b = ggplot(data=df1_sub, aes(x=tmax_f,y=freq,  fill = tmax_f)) + 
  geom_bar(width = 0.9, stat = "identity") +
  labs(title = "Total Cardiovascular related ER visits 2013-2020",
       x = "TMax (F)",
       y = "Freq") + 
  scale_y_continuous(labels = comma) +
  scale_fill_viridis_c(option = "magma") + 
  facet_grid(condition ~ Year, scales="free") +
  theme_bw() +
  guides(fill = "none")  

plot_b


plot_c = ggplot(data=df1_sub, aes(x=tmin_f,y=freq,  fill = tmin_f)) + 
  geom_bar(width = 0.9, stat = "identity") +
  labs(title = "Total Cardiovascular related ER visits 2013-2020",
       x = "TMin (F)",
       y = "Freq") + 
  scale_y_continuous(labels = comma) +
  scale_fill_viridis_c(option = "magma") + 
  facet_grid(condition ~ Year, scales="free") +
  theme_bw() +
  guides(fill = "none")  

plot_c

library(RColorBrewer)

#############PLOT RATES PER 100,000 by condition ####################
df1_plot = readRDS(here::here("df1sub_condition_02_05_24.RDS"))


plot_bar <- ggplot(data = tempER, aes(x = avg_temp_f, y = as.numeric(ERrate1), fill=avg_temp_f)) +
  geom_bar(width = 0.9, stat = "identity", position = "dodge", fill = "orange") +
  labs(title = "Cardiovascular related ER visits per 100k 2013-2020",
       x = "Average temp in zipcode (F)",
       y = "Rate per 100k per zipcode") +
  theme_minimal()  +
  theme(legend.position="none") +
  xlim(40,80)

print(plot_bar)


plot_line <- ggplot(data = tempER, aes(x = avg_temp_f, y = as.numeric(ERrate1))) +
    geom_point() +  
  geom_smooth(formula = y ~ splines::bs(x, 5), se = TRUE) +
    labs(title = "Cardiovascular related ER visits per 100k 2013-2020",
       x = "Average temp in zipcode (F)",
       y = "Rate per 100k per zipcode") +
  theme_minimal()  +
  theme(legend.position="none") +
  xlim(40,80)

print(plot_line)
#box plot

o = ordered(tempER$templabel, levels = c("43.0 - 49.8","49.9 - 56.5","56.6 - 63.2",
                                         "63.3 - 69.8","69.9 - 78.0"))

boxplot(ERrate1 ~ o, data = tempER)

boxplot = ggplot(tempER, aes(x=templabel, y=ERrate1, fill=ERrate1)) + 
  geom_boxplot(fill="orange", alpha=0.2) + 
  scale_fill_manual(values=c("#69b3a2", "grey")) +
  theme(legend.position = "none") +
  xlab("Average temp in zipcode (°F)") + 
  ylab("ER Visit Rate per 100k population") + 
  theme_bw() 

boxplot



# 
# plot_d
# 
# plot_e = ggplot(df1_plot, aes(x = tmax_f, y = RatePer100k, fill = RatePer100k)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   labs(title = "Frequency of CV related hospitalizations 2013-2020",
#        x = "Tmax",
#        y = "Rate per 100k",
#        fill="Condition") +
#   theme_minimal() +
#   scale_fill_brewer(palette = "Dark2", name = "Condition", labels = c("cerebrovascular disease", "dysrhythmia", "heart failure", "ischemic heart disease", "peripheral arterial disease", "pulmonary embolism"))
# 
# 
# plot_e
