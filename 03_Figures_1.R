#Creating figures Part II --
#Making rates instead of absolute numbers
#Feb 5 2024

rm(list=ls())
source("0_00_packages_to_load.R")
source("00_functions1.R")
library(scales)

newpath_casecontrol = "data_case control/"
newpath_population = "data_population/"


df1 = readRDS(here::here(paste0(newpath_casecontrol, "case_uniqueID_census_temp_01_06_24.rds")))

###MAKING DATASETS FOR FIGURES######

# using tmaps
# Code adapted from http://zevross.com/blog/2018/10/02/creating-beautiful-demographic-maps-in-r-with-the-tidycensus-and-tmap-packages/

census_api_key("43433014cdd6694b4179f307c1b0361e4b21a382", overwrite = FALSE, install = FALSE)

gini_county =  get_acs("zcta", table = "B19083", year = 2017,
                       output = "tidy", state = "CA", geometry = TRUE, cache_table = TRUE) %>%
  select(-moe)

gini_county = gini_county %>%
  rename(zip = GEOID)

df1_temp = df1 %>%
  mutate(tmax = signif(tmx, digits=2)) %>%
  select(date, Year, zip, dx_prin, tmax, condition) %>%
  mutate(zip=as.integer(zip)) %>%
  group_by(condition, zip, Year, tmax) %>%
  summarise(condition_total = n())

#load population dataset
pop_data = read.csv(here::here(paste0(newpath_population, "population_data_2020.csv")))
pop_data = pop_data %>%
  select(-X, -NAME, -variable) %>%
  rename(zip = GEOID,
         pop = estimate)


df1_temp_1 = df1_temp %>%
  group_by(condition, tmax, zip) %>%
  summarise(subtotal = n()) %>%
  ungroup() %>%
  merge(pop_data, by="zip") %>%
  select(-moe) %>%
  ungroup() %>%
  mutate(rate = subtotal/pop)

df1_temp_2 = df1_temp_1 %>%
  group_by(condition) %>%
  summarise(total_rate = sum(rate)) %>%
  mutate(rate100k = total_rate*100000) %>%
  mutate(tmax_f = (9/5)*tmax + 32) %>%
  filter(!is.na(tmax_f)) %>%
  filter(rate100k != Inf) %>%
  filter(rate100k != -Inf)

saveRDS(df1_temp_2, "df1sub_condition_02_05_24.RDS")

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

###################FIGURE 1##################
####MAP OF ER VISITS VS GINI COEFF IN CA#####


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

###################FIGURE 2##################
####RATES PER 100k by conditions 


plot_a <- ggplot(data = df1_temp_2, aes(x = as.numeric(tmax_f), y = as.numeric(rate100k), fill = as.factor(condition))) +
  geom_bar(width = 0.9, stat = "identity", position = "dodge") +
  labs(title = "Cardiovascular related ER visits per 100k 2013-2020",
       x = "TMax (F)",
       y = "Rate per 100k") +
  theme_bw()


plot_a



ggplot(df1_temp_2, aes(x = factor(zip), y = rate_per_100k)) +
  geom_bar(stat = "identity") +
  labs(title = "Rates per 100,000 by ZIP Code", x = "ZIP Code", y = "Rate per 100,000") +
  theme_minimal()


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