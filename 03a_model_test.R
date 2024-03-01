#Model Running
#DO YEAR BY YEAR, AS A TEST
#Testing linear, spline, dlnm with lag and var knots to find minimum AIC
# 
# ---
#   title: "Conditional logistic regression for CV related disorders linear, splines, dnlm"
# output:
#   html_document: 
#   toc: yes
# pdf_document: default
# editor_options: 
#   chunk_output_type: console
# ---

rm(list=ls())

# 1a Declare root directory, folder locations and load essential stuff
path1 = "data_casecontrol_lag_wide/"
path1_intermediate = "data_casecontrol_lag_wide/intermediate files/"
functions.folder = "functions.folder/"
source("0_00_packages_to_load.R")
source("00_functions1.R")
source(here::here(paste0(functions.folder, "00_frequentist_models_and_functions.R")))

# Load case crossover lagged data in wide form
df1 = readRDS(here::here(paste0(path1, "wide_control_lagtemp_COMPLETE.rds")))
df2 = readRDS(here::here(paste0(path1, "wide_control_lagpm25_COMPLETE.rds")))

head(df1)
head(df2)



 
# # # Frequentist conditional logistic with linear temperature on day of ER visit     
# fit.clr <- clogit(ck ~ lag0 + strata(nr),
#                   data = df1)
# 
# plot(anova(fit.clr))  # Plot the model's ANOVA table


# # Regression coefficient
# round(summary(fit.clr)$coef, 4)
# #lag0 - OR: 1.00 -- also true for 2013
# 
# OR.CI <- cbind("OR" = exp(coef(fit.clr)),
#                exp(confint(fit.clr)))
# round(OR.CI, 3)



# Find temperature stats for use later
temp_mean = df1 %>%
  summarise(mean(lag0, na.rm = TRUE)) %>%
  as.numeric

temp_min = df1 %>%
  summarise(min(lag0, na.rm = TRUE)) %>%
  as.numeric

temp_max = df1 %>%
  summarise(max(lag0, na.rm = TRUE)) %>%
  as.numeric

temp_seq=seq(from=temp_min, to=temp_max,length.out = 200)
temp_data = data.frame(`lag0`=temp_seq, nr=1)




# Form cross-basis for dnlm from lag and var values

exp_matrix = df1 %>%
  select(contains("lag"))

cb.temp.var3.lag2 <- crossbasis_form(exp_matrix,3,2)
cb.temp.var3.lag3 <- crossbasis_form(exp_matrix,3,3)


cb.temp.var3.lag4 <- crossbasis_form(exp_matrix,3,4)
cb.temp.var3.lag5 <- crossbasis_form(exp_matrix,3,5)
cb.temp.var4.lag2 <- crossbasis_form(exp_matrix,4,2)
cb.temp.var4.lag3 <- crossbasis_form(exp_matrix,4,3)
cb.temp.var4.lag4 <- crossbasis_form(exp_matrix,4,4)
cb.temp.var4.lag5 <- crossbasis_form(exp_matrix,4,5)


# CLOGISTIC MODEL (FREQUENTIST)


library(survival)

#initial models
mod.freq.linear.temp        = clogit_run(df1, fml.linear.temp)
mod.freq.spline.3deg        = clogit_run(df1,fml.spline.3deg)
mod.freq.spline.4deg        = clogit_run(df1,fml.spline.4deg)
mod.freq.spline.5deg        = clogit_run(df1,fml.spline.5deg)


model.freq.spline.3deg.adj = clogit_run(df1, fml.spline.3deg.adj)

predictions <- predict(mod.freq.linear.temp, newdata = new_data, type = "response")


library(forestplot)
coef_data <- as.data.frame(coef(mod.freq.spline.5deg))
ci_data <- as.data.frame(confint(mod.freq.spline.5deg))

coef_data = coef_data %>% 
  rename(coef = "coef(mod.freq.spline.5deg)") %>%
  mutate(OR=exp(coef))%>%
  filter(!is.na(coef))

ci_data = ci_data %>%
  rename(lower = "2.5 %", 
         upper = "97.5 %") %>%
  mutate(lowerCI=exp(lower),
         upperCI=exp(upper))

xticks <- seq(from = 0.5, to = 2, by = 0.2)
xtlab <- rep(c(TRUE, FALSE), length.out = length(xticks))
attr(xticks, "labels") <- xtlab
ORtext = c("df = 1", "df = 2", "df = 3", "df = 4", "df = 5")

tab = cbind(coef_data, ci_data)

forestplot::forestplot(
  mean = coef_data$OR,
  lower = ci_data$lowerCI,
  upper = ci_data$upperCI,
  labeltext = ORtext,
  xticks = xticks,
  zero = 1,
)



# Extract spline terms
spline_terms <- model.matrix(mod.freq.spline.5deg)[, grepl("^spline_", colnames(model.matrix(mod.freq.spline.5deg)))]

# Create partial dependence plots
for (i in 1:ncol(spline_terms)) {
  ggplot(data.frame(spline_term = spline_terms[, i]), aes(x = spline_term)) +
    geom_smooth(aes(y = fitted(fit)), method = "loess") +
    labs(title = paste("Partial Dependence Plot for Spline Term", i))
}

g = base.plot <- ggplot(df1, aes(x = lag0,y = ck)) +
  geom_point(alpha = 0.3) + theme_bw()

g

g = base.plot + 
  geom_smooth(method = 'lm', formula = y ~ poly(x,3, raw = T)) + 
  labs(title = "Polynomial Regression of Dis on Nox")



#memory reached for these models
mod.freq.dnlm.var3.lag2     = clogit_run(df1,fml.freq.dnlm('cb.temp.var3.lag2'))
mod.freq.dnlm.var3.lag2     = clogit_run(df1,ck ~ survival::strata(nr) + cb.temp.var3.lag3
                                         
                                         
                                         )
#mod.freq.dnlm.var3.lag2.alt = clogit_run_alt(dat.complete,fml.freq.dnlm.alt('cb.temp.var3.lag2'))
mod.freq.dnlm.var3.lag3     = clogit_run(dat.complete,fml.freq.dnlm('cb.temp.var3.lag3'))
mod.freq.dnlm.var3.lag4     = clogit_run(dat.complete,fml.freq.dnlm('cb.temp.var3.lag4'))
mod.freq.dnlm.var3.lag5     = clogit_run(dat.complete,fml.freq.dnlm('cb.temp.var3.lag5'))
mod.freq.dnlm.var4.lag2     = clogit_run(dat.complete,fml.freq.dnlm('cb.temp.var4.lag2'))
mod.freq.dnlm.var4.lag3     = clogit_run(dat.complete,fml.freq.dnlm('cb.temp.var4.lag3'))
mod.freq.dnlm.var4.lag4     = clogit_run(dat.complete,fml.freq.dnlm('cb.temp.var4.lag4'))
mod.freq.dnlm.var4.lag5     = clogit_run(dat.complete,fml.freq.dnlm('cb.temp.var4.lag5'))


# PLOT RESULTS FROM FREQ
##1. Linear
print(mod.freq.linear.temp)
##2. Spline with 3 deg freedom
print(mod.freq.spline.3deg)
##3. Spline with 4 deg freedom
print(mod.freq.spline.4deg)

print(mod.freq.spline.5deg)

#ADJUSTED MODELS
print(mod.freq.spline.3deg)
print(model.freq.spline.3deg.adj)



plot(df1$lag, xlab = " (in)", ylab = "Weight (lb)")
summary(fm1 <- lm(weight ~ ns(height, df = 5), data = women))
p1 = predict(fml.spline.3deg, df1$lag0)


##4. Compare AIC values for each of the models

aic.mod.freq.linear.temp    = extractAIC(mod.freq.linear.temp) #same - 3236392
aic.mod.freq.spline.3deg    = extractAIC(mod.freq.spline.3deg) #same - 3236392
# aic.mod.freq.spline.3deg.alt    = extractAIC(mod.freq.spline.3deg.alt)
aic.mod.freq.spline.4deg        = extractAIC(mod.freq.spline.4deg) # 3236393





#plotting splines

base.plot <- ggplot(Boston, aes(x = dis,y = nox)) +
  geom_point(alpha = 0.3) + 
  coord_cartesian(ylim = c(0.3, 0.9)) + theme_bw()