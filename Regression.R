##### load data og pakker #####
# Load necessary libraries
library(plm)
library(stargazer)
library(data.table)
library(rugarch)
library(readxl)
library(lubridate)
library(lmtest)
library(dplyr)
library(sandwich)
library(lubridate)
library(urca)
##plot
library(ggplot2)
library(patchwork)

library(knitr)
library(kableExtra)
library(prais)

library(tidyr)
library(tibble)

library(tseries)
library(reporttools)

library(xts)
# Set working directory and read in data
setwd("/Users/magnushovmand/Dropbox/UNI/Speciale/Data/ENTSO-E/from_2014")
data <- read_excel("daily_df.xlsx")
#data_1diff <- read_excel("daily_df_1dif.xlsx")
setwd("/Users/magnushovmand/Desktop/Elasticitet/")
names(data)[names(data) == 'fourier_h=7_cos'] <- 'fourier_h_7_cos'
names(data)[names(data) == 'fourier_h=7_sin'] <- 'fourier_h_7_sin'
names(data)[names(data) == 'fourier_h=365.25_cos'] <- 'fourier_h_365_cos'
names(data)[names(data) == 'fourier_h=365.25_sin'] <- 'fourier_h_365_sin'

for (var in c("solar_ur", "offshore_ur", "onshore_ur")) {
  new_var_name <- paste("log_", var, sep = "")
  data[[new_var_name]] <- log(data[[var]]) * 100
  
  # Count and print the number of NAs in the new variable
  na_count <- sum(is.na(data[[new_var_name]]))
  print(paste("Number of NAs in", new_var_name, ":", na_count))
}

data$region_dummy <- ifelse(data$region == "DK_1", 1, 0)

#creating dummies
years <- 2016:2023
months <- 2:12

data[paste0("D_", months)] <- sapply(months, function(month) as.integer(format(data$date, "%m") == sprintf("%02d", month)))
data[paste0("D_", years)] <- sapply(years, function(year) as.integer(format(data$date, "%Y") == year))

data_pre_2021 <- subset(data, as.numeric(format(data$date, "%Y")) <= 2020)
data_from_2021 <- subset(data, as.numeric(format(data$date, "%Y")) > 2020)

data_pre_2021_endo <- data_pre_2021
# data_DK1 <- subset(data, region == "DK_1")
# data_DK2 <- subset(data, region == "DK_2")
# 
# data_pre_2021_DK1 <- subset(data_pre_2021, region == "DK_1")
# data_pre_2021_DK2 <- subset(data_pre_2021, region == "DK_2")
# 
# data_from_2021_DK1 <- subset(data_from_2021, region == "DK_1")
# data_from_2021_DK2 <- subset(data_from_2021, region == "DK_2")

kilde_farve <- c("Sol pen" = "orange", "Havvind pen" = "blue", "Landvind pen" = "brown")
kilde_farve_full <- c("solar_penetration" = "orange", "offshore_penetration" = "blue", "onshore_penetration" = "brown")
names_values <- c("Sol pen", "Havvind pen", "Landvind pen")

# data_pre_2021_summer <- subset(data_pre_2021, month(date) %in% 6:8)
# data_pre_2021_winter <- subset(data_pre_2021, month(date) %in% c(1, 2, 12))

##### Definering af parameter series til regression####

target_vars <- c('log_solar_ur', 'log_offshore_ur', 'log_onshore_ur', 'solar_uf', 'offshore_uf', 'onshore_uf')
target_vars_temp <- c('solar_ur', 'offshore_ur', 'onshore_ur', 'solar_uf', 'offshore_uf', 'onshore_uf')
first_stage_target_vars <- c("solar_penetration", "offshore_penetration", "onshore_penetration")
first_stage_regressors <- c("p_solar_penetration", "p_offshore_penetration", "p_onshore_penetration", "temperature", "region_dummy", "is_weekend")
vars_non_season <- c("solar_penetration_hat", "offshore_penetration_hat", "onshore_penetration_hat", "temperature", "region_dummy", "is_weekend")
vars_non_season_endo <- c("solar_penetration_hat", "offshore_penetration_hat", "onshore_penetration_hat", "fossil_gas_penetration", "net_export_penetration", "load", "temperature", "gas_price", "region_dummy", "is_weekend")
first_stage_vars_non_season_endo <- c("p_solar_penetration", "p_offshore_penetration", "p_onshore_penetration", "fossil_gas_penetration", "net_export_penetration", "load", "temperature", "gas_price", "region_dummy", "is_weekend")
vars_non_season_region <- c("solar_penetration_hat", "offshore_penetration_hat", "onshore_penetration_hat", "temperature", "is_weekend") 
vars_non_season_OLS <- c("solar_penetration", "offshore_penetration", "onshore_penetration", "temperature", "region_dummy", "is_weekend")
d_years_pre <- c("D_2016","D_2017","D_2018","D_2019","D_2020")#, 
d_years_from <- c("D_2022", "D_2023")
d_months <- c("D_2","D_3","D_4","D_5","D_6","D_7","D_8","D_9","D_10","D_11","D_12")
fourier_vars <- c("fourier_h_7_cos", "fourier_h_7_sin", "fourier_h_365_cos", "fourier_h_365_sin")

interaction_terms_pre_2021 <- vector("list", length = length(d_years_pre) * length(fourier_vars))
index <- 1

for (d_year in d_years_pre) {
  for (fourier_var in fourier_vars[3:4]) {
    interaction_terms_pre_2021[[index]] <- paste(d_year, fourier_var, sep = "*")
    index <- index + 1}
}

for (d_month in d_months) {
  for (fourier_var in fourier_vars[1:2]) {
    interaction_terms_pre_2021[[index]] <- paste(d_month, fourier_var, sep = "*")
    index <- index + 1}
}

interaction_terms_from_2021 <- vector("list", length = length(d_years_from) * length(fourier_vars))
index <- 1

for (d_year in d_years_from) {
  for (fourier_var in fourier_vars[3:4]) {
    interaction_terms_from_2021[[index]] <- paste(d_year, fourier_var, sep = "*")
    index <- index + 1}
}

for (d_month in d_months) {
  for (fourier_var in fourier_vars[1:2]) {
    interaction_terms_from_2021[[index]] <- paste(d_month, fourier_var, sep = "*")
    index <- index + 1}
}

predictors_pre_2021 <- c(vars_non_season, interaction_terms_pre_2021, d_years_pre)
first_stage_predictors_pre_2021 <- c(first_stage_regressors, interaction_terms_pre_2021, d_years_pre)
first_stage_predictors_pre_2021_endo <- c(first_stage_vars_non_season_endo, interaction_terms_pre_2021, d_years_from)
predictors_pre_2021_OLS <- c(vars_non_season_OLS, interaction_terms_pre_2021, d_years_pre)
first_stage_predictors_from_2021 <- c(first_stage_regressors, interaction_terms_from_2021, d_years_from)
predictors_from_2021 <- c(vars_non_season, interaction_terms_from_2021, d_years_from)
predictors_region <- c(vars_non_season_region, interaction_terms_pre_2021, d_years_pre)
predictors_robust_endo <- c(vars_non_season_endo, interaction_terms_pre_2021, d_years_pre)

results <- list() 

##### Defining regressions and functions#####

time_series_regression <- function(response, predictors, data) {

  formula <- as.formula(
    paste(response, "~", paste(predictors, collapse=" + "))
  )

  # Estimate the linear model
  lm_fit <- lm(formula, data = data, na.action = na.exclude)

  # Extract the fitted values
  fitted_values <- fitted(lm_fit)

  # Extract statistics
  n_obs <- length(fitted_values)
  r_squared <- summary(lm_fit)$r.squared
  r_squared_adj <- summary(lm_fit)$adj.r.squared
  f_statistic <- summary(lm_fit)$fstatistic['value']

  hac_se <- sqrt(diag(vcovHAC(lm_fit)))
  coeftest_hac <- coeftest(lm_fit, vcov = vcovHAC(lm_fit))
  
  return(list(coeftest = coeftest_hac, n_obs = n_obs, r_squared = r_squared,
              r_squared_adj = r_squared_adj, f_statistic = f_statistic,
              fitted_values = fitted_values, robust_se = hac_se))
}

panel_fe_regression <- function(response, predictors, data) {
  
  formula <- as.formula(
    paste(response, "~", paste(predictors, collapse=" + "))
  )
  
  # Estimate the fixed effects model using lm
  fe_fit <- plm(formula, data = data, model = "pooling", index = c("region", "date"), na.action = na.exclude) #, effect = "individual"
  
  fitted_values <- fitted(fe_fit)
  #individual_effects <- fixef(fe_fit) #, effect = "individual"
  #time_effects <- fixef(fe_fit, effect = "time")
  #fitted_values <- fitted_values + individual_effects
  
  # Extract statistics
  n_obs <- nobs(fe_fit)
  r_squared <- summary(fe_fit)$r.squared
  r_squared_within <- summary(fe_fit)$r.squaredWithin
  f_statistic <- summary(fe_fit)$fstatistic['value']
  
  coeftest_hac <- coeftest(fe_fit, vcov.=function(x) vcovNW(x, type="HC3"))
  test_auto_correlation <- pbgtest(fe_fit, order = NULL, type = c("Chisq", "F"))
  return(list(coeftest = coeftest_hac, n_obs = n_obs, r_squared = r_squared,
              r_squared_within = r_squared_within, f_statistic = f_statistic,
              fitted_values = fitted_values, test_auto_correlation = test_auto_correlation))
}

cumulative_count <- function(x) {
  sort_x <- sort(x, decreasing = FALSE)
  data.frame(value = sort_x, cumcount = seq_along(sort_x))
}

# Define the categorize function
categorize <- function(x, one_third, two_thirds) {
  ifelse(x < one_third, "below_one_third",
         ifelse(x < two_thirds, "between_one_and_two_thirds", "above_two_thirds"))
}

# Define the get_counts function
get_counts <- function(data, split_points) {
  results <- sapply(split_points$variable, function(var) {
    # Extract the cutoff points for the current variable
    cutoffs <- split_points[split_points$variable == var, c("one_third", "two_thirds")]
    
    # Apply the categorize function to the data
    categories <- categorize(data[[var]], cutoffs$one_third, cutoffs$two_thirds)
    
    # Get the count of each category
    table(categories)
  }, simplify = FALSE)
  return(results)
}

##### Running first stage #####

for (var in first_stage_target_vars){
  results[["First stage"]][["pre"]][[var]] = time_series_regression(var, first_stage_predictors_pre_2021, data_pre_2021)
}

for (var in first_stage_target_vars){
  results[["First stage"]][["from"]][[var]] = time_series_regression(var, first_stage_predictors_from_2021, data_from_2021)
}

## Print first stage for pre 2021
stargazer(results[["First stage"]][["pre"]][[1]][[1]], results[["First stage"]][["pre"]][[2]][[1]], results[["First stage"]][["pre"]][[3]][[1]],
          title = "First stage regression",
          table.placement = "H",
          label="tab:first_stage",
          header = FALSE,
          type = "latex",
          column.labels = c("Sol", "Havvind", "Landvind"),
          column.sep.width = "10pt",
          align = TRUE,  # Assuming you want centered alignment. Adjust as needed.
          dep.var.labels.include = FALSE,
          model.names = FALSE,
          digits=2,
          out = "firststage_regression.tex",
          covariate.labels = c("Prædikteret sol", "Prædikteret havvind", "Prædikteret landvind", "Temperatur"),
          keep = first_stage_regressors[1:4], 
          add.lines = list(
            c("Antal observationer", format(results[["First stage"]][["pre"]][[1]][[2]], digits = 2, nsmall = 2), format(results[["First stage"]][["pre"]][[2]][[2]], digits = 2, nsmall = 2), format(results[["First stage"]][["pre"]][[3]][[2]], digits = 2, nsmall = 2)),
            c("Frihedsgrader", format(as.numeric(attr(results[["First stage"]][["pre"]][[1]][["coeftest"]], "df")), digits = 2),
              format(as.numeric(attr(results[["First stage"]][["pre"]][[2]][["coeftest"]], "df")), digits = 2),
              format(as.numeric(attr(results[["First stage"]][["pre"]][[3]][["coeftest"]], "df")), digits = 2)),
            c("R-squared", format(results[["First stage"]][["pre"]][[1]][[3]], digits = 2, nsmall = 2), format(results[["First stage"]][["pre"]][[2]][[3]], digits = 2, nsmall = 2), format(results[["First stage"]][["pre"]][[3]][[3]], digits = 2, nsmall = 2)),
            c("Justeret R-squared", format(results[["First stage"]][["pre"]][[1]][[4]], digits = 2, nsmall = 2), format(results[["First stage"]][["pre"]][[2]][[4]], digits = 2, nsmall = 2), format(results[["First stage"]][["pre"]][[3]][[4]], digits = 2, nsmall = 2)),
            c("F Statistik", format(results[["First stage"]][["pre"]][[1]][[5]], digits = 2, nsmall = 2), format(results[["First stage"]][["pre"]][[2]][[5]], digits = 2, nsmall = 2), format(results[["First stage"]][["pre"]][[3]][[5]], digits = 2, nsmall = 2)),
            c("Fixed effekt", "Individ", "Individ", "Individ"),
            c("Standardfejl", "HAC", "HAC", "HAC"),
            c("Års dummies", "Ja", "Ja", "Ja"),
            c("Årlige og ugentlige fouriertransformationer", "Ja", "Ja", "Ja")
          )
)

## Print first stage from 2021
stargazer(results[["First stage"]][["from"]][[1]][[1]], results[["First stage"]][["from"]][[2]][[1]], results[["First stage"]][["from"]][[3]][[1]],
          title = "First stage regression",
          table.placement = "H",
          label="tab:first_stage",
          header = FALSE,
          type = "latex",
          column.labels = c("Sol", "Havvind", "Landvind"),
          column.sep.width = "10pt",
          align = TRUE,  # Assuming you want centered alignment. Adjust as needed.
          dep.var.labels.include = TRUE,
          model.names = TRUE,
          digits=2,
          out = "firststage_regression.tex",
          covariate.labels = c("Prædikteret sol", "Prædikteret havvind", "Prædikteret landvind", "Temperatur"),
          keep = first_stage_regressors[1:4], 
          add.lines = list(
            c("Antal observationer", format(results[["First stage"]][["from"]][[1]][[2]], digits = 2, nsmall = 2), format(results[["First stage"]][["from"]][[2]][[2]], digits = 2, nsmall = 2), format(results[["First stage"]][["from"]][[3]][[2]], digits = 2, nsmall = 2)),
            c("Frihedsgrader", format(as.numeric(attr(results[["First stage"]][["from"]][[1]][["coeftest"]], "df")), digits = 2),
              format(as.numeric(attr(results[["First stage"]][["from"]][[2]][["coeftest"]], "df")), digits = 2),
              format(as.numeric(attr(results[["First stage"]][["from"]][[3]][["coeftest"]], "df")), digits = 2)),
            c("R-squared", format(results[["First stage"]][["from"]][[1]][[3]], digits = 2, nsmall = 2), format(results[["First stage"]][["from"]][[2]][[3]], digits = 2, nsmall = 2), format(results[["First stage"]][["from"]][[3]][[3]], digits = 2, nsmall = 2)),
            c("Justeret R-squared", format(results[["First stage"]][["from"]][[1]][[4]], digits = 2, nsmall = 2), format(results[["First stage"]][["from"]][[2]][[4]], digits = 2, nsmall = 2), format(results[["First stage"]][["from"]][[3]][[4]], digits = 2, nsmall = 2)),
            c("F Statistik", format(results[["First stage"]][["from"]][[1]][[5]], digits = 2, nsmall = 2), format(results[["First stage"]][["from"]][[2]][[5]], digits = 2, nsmall = 2), format(results[["First stage"]][["from"]][[3]][[5]], digits = 2, nsmall = 2)),
            c("Standardfejl", "HAC", "HAC", "HAC"),
            c("Års dummies", "Ja", "Ja", "Ja"),
            c("Årlige og ugentlige fouriertransformationer", "Ja", "Ja", "Ja")
          )
)

data_pre_2021$solar_penetration_hat = results[["First stage"]][[1]][[1]][["fitted_values"]]
data_pre_2021$offshore_penetration_hat = results[["First stage"]][[1]][[2]][["fitted_values"]]
data_pre_2021$onshore_penetration_hat = results[["First stage"]][[1]][[3]][["fitted_values"]]

data_from_2021$solar_penetration_hat = results[["First stage"]][[2]][[1]][["fitted_values"]]
data_from_2021$offshore_penetration_hat = results[["First stage"]][[2]][[2]][["fitted_values"]]
data_from_2021$onshore_penetration_hat = results[["First stage"]][[2]][[3]][["fitted_values"]]

##### plot scatter af first stage instrument og afhængig ####
scatter_solar <- ggplot(data_pre_2021, aes(x = p_solar_penetration, y = solar_penetration_hat)) +
  geom_point(alpha = 0.2, color = "gray") +
  geom_abline(slope = 1, linetype = "dotted") +
  geom_abline(slope = results[["First stage"]][["pre"]][[1]][[1]][[2]], color = "orange") +
  labs(x = "Prædikterede penetration", y = "Faktisk penetration", title = "Sol") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white", colour = "white")) +
  scale_x_continuous(limits = c(0, 150)) +
  scale_y_continuous(limits = c(0, 150)) +
  annotate("text", x = 0, y = 150, 
           label = expression(R^2 == 0.95), hjust = 0, vjust = 1, 
           size = 5, color = "black", family = "mono", fontface = "bold") +
  annotate("text", x = 0, y = 125, 
           label = expression(hat(beta)[1] == 0.95), hjust = 0, vjust = 1, 
           size = 5, color = "black", family = "mono", fontface = "bold")

scatter_havvind <- ggplot(data_pre_2021, aes(x = p_offshore_penetration, y = offshore_penetration_hat)) +
  geom_point(alpha = 0.2, color = "gray") +
  geom_abline(slope = 1, linetype = "dotted") +
  geom_abline(slope = results[["First stage"]][["pre"]][[2]][[1]][[3]], color = "blue") +
  labs(x = "Prædikterede penetration", y = "Faktisk penetration", title = "Havvind") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white", colour = "white")) +
  scale_x_continuous(limits = c(0, 150)) +
  scale_y_continuous(limits = c(0, 150)) +
  annotate("text", x = 0, y = 150,
           label = expression(R^2 == 0.95), hjust = 0, vjust = 1, 
           size = 5, color = "black", family = "mono", fontface = "bold") +
  annotate("text", x = 0, y = 125, 
           label = expression(hat(beta)[1] == 0.95), hjust = 0, vjust = 1, 
           size = 5, color = "black", family = "mono", fontface = "bold")

scatter_landvind <- ggplot(data_pre_2021, aes(x = p_onshore_penetration, y = onshore_penetration_hat)) +
  geom_point(alpha = 0.2, color = "gray") +
  geom_abline(slope = 1, linetype = "dotted") +
  geom_abline(slope = results[["First stage"]][["pre"]][[3]][[1]][[4]], color = "brown") +
  labs(x = "Prædikterede penetration", y = "Faktisk penetration", title = "Landvind") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white", colour = "white")) +
  scale_x_continuous(limits = c(0, 150)) +
  scale_y_continuous(limits = c(0, 150)) +
  annotate("text", x = 0, y = 150, 
           label = expression(R^2 == 0.97), hjust = 0, vjust = 1, 
           size = 5, color = "black", family = "mono", fontface = "bold") +
  annotate("text", x = 0, y = 125, 
           label = expression(hat(beta)[1] == 0.92), hjust = 0, vjust = 1, 
           size = 5, color = "black", family = "mono", fontface = "bold")


ggsave("scatter_solar.png", plot = scatter_solar, units = "in", dpi = 300)
ggsave("scatter_havvind.png", plot = scatter_havvind, units = "in", dpi = 300)
ggsave("scatter_landvind.png", plot = scatter_landvind, units = "in", dpi = 300)

##### Splitting dataset into subsets based on years and region ####
# data_DK1 <- subset(data, region == "DK_1")
# data_DK2 <- subset(data, region == "DK_2")

data_pre_2021_DK1 <- subset(data_pre_2021, region == "DK_1")
data_pre_2021_DK2 <- subset(data_pre_2021, region == "DK_2")

data_from_2021_DK1 <- subset(data_from_2021, region == "DK_1")
data_from_2021_DK2 <- subset(data_from_2021, region == "DK_2")

data_pre_2021_summer <- subset(data_pre_2021, month(date) %in% 6:8)
data_pre_2021_winter <- subset(data_pre_2021, month(date) %in% c(1, 2, 12))
##### printing summary statistics of period pre 2021 #####

# vars4 <- data_pre_2021[, c("solar_ur", "offshore_ur", "onshore_ur", "solar_uf", "offshore_uf", "onshore_uf", "solar_penetration", "offshore_penetration", "onshore_penetration", "temperature")]
# vars4 <- as.data.frame(vars4)
# cap4 <- "Summary tabel for data i perioden 2015 til 2020 DK1 og DK2 samlet"
# tableContinuous(vars = vars4, cap = cap4, lab = "tab::Summary 2015 til 2020 DK1 og DK2", longtable = FALSE)
# 
# 
# summary(vars4)
# 
# data_pre_2021[is.na(data_pre_2021$solar_ur), ]

na_counts <- data_pre_2021 %>%
  summarise(across(everything(), ~sum(is.na(.))))

# Udskriv resultatet
print(na_counts)



##### Resultater - før og efter 2021 både DK1 og DK2 #####

# # køre regression
# result <- list()
# for (var in target_vars) {
#   result[[var]] <- panel_fe_regression(var, predictors, data)
#   print(var)
# }
# 
# #print table
# stargazer(result[[1]][[1]],result[[2]][[1]],result[[3]][[1]], result[[4]][[1]],result[[5]][[1]],result[[6]][[1]],
#           title = "Regresultsion for perioden 2015 til 2023 Endhedprisen og capture raten",
#           table.placement = "H",
#           label="tab:result_ur",
#           header = FALSE,
#           type = "latex",
#           column.labels = c("Sol", "Havvind", "Landvind", "Sol", "Havvind", "Landvind"),
#           column.sep.width = "10pt",
#           align = TRUE,
#           dep.var.labels.include = FALSE,
#           model.names = FALSE,
#           digits=2,
#           out = "firststage_regssion.tex",
#           covariate.labels = predictors_second_stage <- c("Sol pen", "Havvind pen", "Landvind pen"),
#           omit = c("fourier_h_7_cos", "fourier_h_7_sin", "fourier_h_365_cos", "fourier_h_365_sin", "is_weekend", "temperature", "D_2016","D_2017","D_2018","D_2019","D_2020","D_2021","D_2022","D_2023", "Constant"),
#           add.lines = list(
#             c("Antal observationer", format(result[[1]][[2]], digits = 2, nsmall = 2), format(result[[2]][[2]], digits = 2, nsmall = 2), format(result[[3]][[2]], digits = 2, nsmall = 2), format(result[[4]][[2]], digits = 2, nsmall = 2), format(result[[5]][[2]], digits = 2, nsmall = 2), format(result[[6]][[2]]), digits = 2, nsmall = 2),
#             c("R-squared", format(result[[1]][[3]], digits = 2, nsmall = 2), format(result[[2]][[3]], digits = 2, nsmall = 2), format(result[[3]][[3]], digits = 2, nsmall = 2), format(result[[4]][[3]], digits = 2, nsmall = 2), format(result[[5]][[3]], digits = 2, nsmall = 2), format(result[[6]][[3]], digits = 2, nsmall = 2)),
#             c("Justeret R-squared", format(result[[1]][[4]], digits = 2, nsmall = 2), format(result[[2]][[4]], digits = 2, nsmall = 2), format(result[[3]][[4]], digits = 2, nsmall = 2), format(result[[4]][[4]], digits = 2, nsmall = 2), format(result[[5]][[4]], digits = 2, nsmall = 2), format(result[[6]][[4]], digits = 2, nsmall = 2)),
#             c("F Statistik", format(result[[1]][[5]], digits = 2, nsmall = 2), format(result[[2]][[5]], digits = 2, nsmall = 2), format(result[[3]][[5]], digits = 2, nsmall = 2), format(result[[4]][[5]], digits = 2, nsmall = 2), format(result[[5]][[5]], digits = 2, nsmall = 2), format(result[[6]][[5]], digits = 2, nsmall = 2)),
#             c("Standardfejl", "HAC", "HAC", "HAC", "HAC", "HAC", "HAC"),
#             c("Fourier transformationer", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja"),
#             c("Weekend og års dummies", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja")))

##### Resultater - før 2021 #####

#køre regression
for (var in target_vars) {
  results[["Pre 2021"]][[var]] <- time_series_regression(var, predictors_pre_2021, data_pre_2021)
}

#print tabel
stargazer(results[["Pre 2021"]][[1]][[1]],results[["Pre 2021"]][[2]][[1]],results[["Pre 2021"]][[3]][[1]], results[["Pre 2021"]][[4]][[1]],results[["Pre 2021"]][[5]][[1]],results[["Pre 2021"]][[6]][[1]],
          title = "Fixed effect resultater for perioden 2015 til 2020 for penetrations raternes effekt på enhedspris og relative pris",
          table.placement = "H",
          label="tab:Results pre 2021",
          header = FALSE,
          type = "latex",
          column.labels = c("Sol", "Havvind", "Landvind", "Sol", "Havvind", "Landvind"),
          column.sep.width = "10pt",
          align = TRUE,
          dep.var.labels.include = FALSE,
          model.names = FALSE,
          digits=2,
          out = "firststage_regression.tex",
          #omit = c(interaction_terms_pre_2021, d_years_pre, "region_dummy", "is_weekend", "constant"),#c("fourier_h_7_cos", "fourier_h_7_sin", "fourier_h_365_cos", "fourier_h_365_sin", "is_weekend", "temperature", "D_2016","D_2017","D_2018","D_2016","D_2019","D_2020","D_2021","D_2022","D_2023"),
          covariate.labels = c("Sol pen", "Havvind pen", "Landvind pen", "Temperatur"),
          keep = vars_non_season[1:4], 
          add.lines = list(
            c("Antal observationer", format(results[["Pre 2021"]][[1]][[2]], digits = 2, nsmall = 2), format(results[["Pre 2021"]][[2]][[2]], digits = 2, nsmall = 2), format(results[["Pre 2021"]][[3]][[2]], digits = 2, nsmall = 2), format(results[["Pre 2021"]][[4]][[2]], digits = 2, nsmall = 2), format(results[["Pre 2021"]][[5]][[2]], digits = 2, nsmall = 2), format(results[["Pre 2021"]][[6]][[2]], digits = 2, nsmall = 2)),
            c("Frihedsgrader", format(as.numeric(attr(results[["Pre 2021"]][["pre"]][[1]][["coeftest"]], "df")), digits = 2),
              format(as.numeric(attr(results[["Pre 2021"]][[2]][["coeftest"]], "df")), digits = 2),
              format(as.numeric(attr(results[["Pre 2021"]][[3]][["coeftest"]], "df")), digits = 2),
              format(as.numeric(attr(results[["Pre 2021"]][[4]][["coeftest"]], "df")), digits = 2),
              format(as.numeric(attr(results[["Pre 2021"]][[5]][["coeftest"]], "df")), digits = 2),
              format(as.numeric(attr(results[["Pre 2021"]][[6]][["coeftest"]], "df")), digits = 2)),
            c("R-squared", format(results[["Pre 2021"]][[1]][[3]], digits = 2, nsmall = 2), format(results[["Pre 2021"]][[2]][[3]], digits = 2, nsmall = 2), format(results[["Pre 2021"]][[3]][[3]], digits = 2, nsmall = 2), format(results[["Pre 2021"]][[4]][[3]], digits = 2, nsmall = 2), format(results[["Pre 2021"]][[5]][[3]], digits = 2, nsmall = 2), format(results[["Pre 2021"]][[6]][[3]], digits = 2, nsmall = 2)),
            c("Justeret R-squared", format(results[["Pre 2021"]][[1]][[4]], digits = 2, nsmall = 2), format(results[["Pre 2021"]][[2]][[4]], digits = 2, nsmall = 2), format(results[["Pre 2021"]][[3]][[4]], digits = 2, nsmall = 2), format(results[["Pre 2021"]][[4]][[4]], digits = 2, nsmall = 2), format(results[["Pre 2021"]][[5]][[4]], digits = 2, nsmall = 2), format(results[["Pre 2021"]][[6]][[4]], digits = 2, nsmall = 2)),
            c("Fixed effekt", "Individ", "Individ", "Individ", "Individ", "Individ", "Individ"),
            c("Standardfejl", "HAC", "HAC", "HAC", "HAC", "HAC", "HAC"),
            c("Års dummies", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja"),
            c("Årlige og ugentlige fouriertransformationer", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja")))

##### Resultater - før 2021 - ikke log #####

#køre regression
for (var in target_vars_temp) {
  results[["Pre 2021 ikke log"]][[var]] <- time_series_regression(var, predictors_pre_2021, data_pre_2021)
}

#print tabel
stargazer(results[["Pre 2021 ikke log"]][[1]][[1]],results[["Pre 2021 ikke log"]][[2]][[1]],results[["Pre 2021 ikke log"]][[3]][[1]],
          title = "Fixed effect resultater for perioden 2015 til 2020 for penetrations raternes effekt på enhedspris og relative pris",
          table.placement = "H",
          label="tab:Results pre 2021 ikke log",
          header = FALSE,
          type = "latex",
          column.labels = c("Sol", "Havvind", "Landvind"),
          column.sep.width = "10pt",
          align = TRUE,
          dep.var.labels.include = FALSE,
          model.names = FALSE,
          digits=2,
          out = "firststage_regression.tex",
          #omit = c(interaction_terms_pre_2021, d_years_pre, "region_dummy", "is_weekend", "constant"),#c("fourier_h_7_cos", "fourier_h_7_sin", "fourier_h_365_cos", "fourier_h_365_sin", "is_weekend", "temperature", "D_2016","D_2017","D_2018","D_2016","D_2019","D_2020","D_2021","D_2022","D_2023"),
          covariate.labels = c("Sol pen", "Havvind pen", "Landvind pen", "Temperatur"),
          keep = vars_non_season[1:4], 
          add.lines = list(
            c("Antal observationer", format(results[["Pre 2021 ikke log"]][[1]][[2]], digits = 2, nsmall = 2), format(results[["Pre 2021 ikke log"]][[2]][[2]], digits = 2, nsmall = 2), format(results[["Pre 2021 ikke log"]][[3]][[2]], digits = 2, nsmall = 2)),
            c("Frihedsgrader", format(as.numeric(attr(results[["Pre 2021 ikke log"]][["pre"]][[1]][["coeftest"]], "df")), digits = 2),
              format(as.numeric(attr(results[["Pre 2021 ikke log"]][[2]][["coeftest"]], "df")), digits = 2),
              format(as.numeric(attr(results[["Pre 2021 ikke log"]][[3]][["coeftest"]], "df")), digits = 2)),
            c("R-squared", format(results[["Pre 2021 ikke log"]][[1]][[3]], digits = 2, nsmall = 2), format(results[["Pre 2021 ikke log"]][[2]][[3]], digits = 2, nsmall = 2), format(results[["Pre 2021 ikke log"]][[3]][[3]], digits = 2, nsmall = 2)),
            c("Justeret R-squared", format(results[["Pre 2021 ikke log"]][[1]][[4]], digits = 2, nsmall = 2), format(results[["Pre 2021 ikke log"]][[2]][[4]], digits = 2, nsmall = 2), format(results[["Pre 2021 ikke log"]][[3]][[4]], digits = 2, nsmall = 2)),
            c("Fixed effekt", "Individ", "Individ", "Individ"),
            c("Standardfejl", "HAC", "HAC", "HAC"),
            c("Års dummies", "Ja", "Ja", "Ja"),
            c("Årlige og ugentlige fouriertransformationer", "Ja", "Ja", "Ja")))

##### Simpel OLS - før 2021 ####
#køre regression
for (var in target_vars) {
  results[["OLS pre 2021"]][[var]] <- time_series_regression(var, predictors_pre_2021_OLS, data_pre_2021)
}

#print tabel
stargazer(results[["OLS pre 2021"]][[1]][[1]],results[["OLS pre 2021"]][[2]][[1]],results[["OLS pre 2021"]][[3]][[1]], results[["OLS pre 2021"]][[4]][[1]],results[["OLS pre 2021"]][[5]][[1]],results[["OLS pre 2021"]][[6]][[1]],
          title = "Fixed effect resultater for perioden 2015 til 2020 for penetrations raternes effekt på enhedspris og relative pris",
          table.placement = "H",
          label="tab:Results pre 2021",
          header = FALSE,
          type = "latex",
          column.labels = c("Sol", "Havvind", "Landvind", "Sol", "Havvind", "Landvind"),
          column.sep.width = "10pt",
          align = TRUE,
          dep.var.labels.include = FALSE,
          model.names = FALSE,
          digits=2,
          out = "firststage_regression.tex",
          #omit = c(interaction_terms_pre_2021, d_years_pre, "region_dummy", "is_weekend", "constant"),#c("fourier_h_7_cos", "fourier_h_7_sin", "fourier_h_365_cos", "fourier_h_365_sin", "is_weekend", "temperature", "D_2016","D_2017","D_2018","D_2016","D_2019","D_2020","D_2021","D_2022","D_2023"),
          covariate.labels = c("Sol pen", "Havvind pen", "Landvind pen", "Temperatur"),
          keep = vars_non_season_OLS[1:4], 
          add.lines = list(
            c("Antal observationer", format(results[["OLS pre 2021"]][[1]][[2]], digits = 2, nsmall = 2), format(results[["OLS pre 2021"]][[2]][[2]], digits = 2, nsmall = 2), format(results[["OLS pre 2021"]][[3]][[2]], digits = 2, nsmall = 2), format(results[["OLS pre 2021"]][[4]][[2]], digits = 2, nsmall = 2), format(results[["OLS pre 2021"]][[5]][[2]], digits = 2, nsmall = 2), format(results[["OLS pre 2021"]][[6]][[2]], digits = 2, nsmall = 2)),
            c("Frihedsgrader", format(as.numeric(attr(results[["OLS pre 2021"]][["pre"]][[1]][["coeftest"]], "df")), digits = 2),
              format(as.numeric(attr(results[["OLS pre 2021"]][[2]][["coeftest"]], "df")), digits = 2),
              format(as.numeric(attr(results[["OLS pre 2021"]][[3]][["coeftest"]], "df")), digits = 2),
              format(as.numeric(attr(results[["OLS pre 2021"]][[4]][["coeftest"]], "df")), digits = 2),
              format(as.numeric(attr(results[["OLS pre 2021"]][[5]][["coeftest"]], "df")), digits = 2),
              format(as.numeric(attr(results[["OLS pre 2021"]][[6]][["coeftest"]], "df")), digits = 2)),
            c("R-squared", format(results[["OLS pre 2021"]][[1]][[3]], digits = 2, nsmall = 2), format(results[["OLS pre 2021"]][[2]][[3]], digits = 2, nsmall = 2), format(results[["OLS pre 2021"]][[3]][[3]], digits = 2, nsmall = 2), format(results[["OLS pre 2021"]][[4]][[3]], digits = 2, nsmall = 2), format(results[["OLS pre 2021"]][[5]][[3]], digits = 2, nsmall = 2), format(results[["OLS pre 2021"]][[6]][[3]], digits = 2, nsmall = 2)),
            c("Justeret R-squared", format(results[["OLS pre 2021"]][[1]][[4]], digits = 2, nsmall = 2), format(results[["OLS pre 2021"]][[2]][[4]], digits = 2, nsmall = 2), format(results[["OLS pre 2021"]][[3]][[4]], digits = 2, nsmall = 2), format(results[["OLS pre 2021"]][[4]][[4]], digits = 2, nsmall = 2), format(results[["OLS pre 2021"]][[5]][[4]], digits = 2, nsmall = 2), format(results[["OLS pre 2021"]][[6]][[4]], digits = 2, nsmall = 2)),
            c("Fixed effekt", "Individ", "Individ", "Individ", "Individ", "Individ", "Individ"),
            c("Standardfejl", "HAC", "HAC", "HAC", "HAC", "HAC", "HAC"),
            c("Års dummies", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja"),
            c("Årlige og ugentlige fouriertransformationer", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja")))

##### Resultater - efter 2021 #####

# Kør regression
for (var in target_vars) {
  results[["From 2021"]][[var]] <- time_series_regression(var, predictors_from_2021, data_from_2021)
}

#print tabel
stargazer(results[["From 2021"]][[1]][[1]],results[["From 2021"]][[2]][[1]],results[["From 2021"]][[3]][[1]], results[["From 2021"]][[4]][[1]],results[["From 2021"]][[5]][[1]],results[["From 2021"]][[6]][[1]],
          title = "Fixed effect resultater for perioden 2015 til 2020 for penetrations raternes effekt på enhedspris og relative pris",
          table.placement = "H",
          label="tab:Results pre 2021",
          header = FALSE,
          type = "latex",
          column.labels = c("Sol", "Havvind", "Landvind", "Sol", "Havvind", "Landvind"),
          column.sep.width = "10pt",
          align = TRUE,
          dep.var.labels.include = FALSE,
          model.names = FALSE,
          digits=2,
          out = "firststage_regression.tex",
          #omit = c(interaction_terms_pre_2021, d_years_pre, "region_dummy", "is_weekend", "constant"),#c("fourier_h_7_cos", "fourier_h_7_sin", "fourier_h_365_cos", "fourier_h_365_sin", "is_weekend", "temperature", "D_2016","D_2017","D_2018","D_2016","D_2019","D_2020","D_2021","D_2022","D_2023"),
          covariate.labels = c("Sol pen", "Havvind pen", "Landvind pen", "Temperatur"),
          keep = vars_non_season[1:4], 
          add.lines = list(
            c("Antal observationer", format(results[["From 2021"]][[1]][[2]], digits = 2, nsmall = 2), format(results[["From 2021"]][[2]][[2]], digits = 2, nsmall = 2), format(results[["From 2021"]][[3]][[2]], digits = 2, nsmall = 2), format(results[["From 2021"]][[4]][[2]], digits = 2, nsmall = 2), format(results[["From 2021"]][[5]][[2]], digits = 2, nsmall = 2), format(results[["From 2021"]][[6]][[2]], digits = 2, nsmall = 2)),
            c("Frihedsgrader", format(as.numeric(attr(results[["From 2021"]][["pre"]][[1]][["coeftest"]], "df")), digits = 2),
              format(as.numeric(attr(results[["From 2021"]][[2]][["coeftest"]], "df")), digits = 2),
              format(as.numeric(attr(results[["From 2021"]][[3]][["coeftest"]], "df")), digits = 2),
              format(as.numeric(attr(results[["From 2021"]][[4]][["coeftest"]], "df")), digits = 2),
              format(as.numeric(attr(results[["From 2021"]][[5]][["coeftest"]], "df")), digits = 2),
              format(as.numeric(attr(results[["From 2021"]][[6]][["coeftest"]], "df")), digits = 2)),
            c("R-squared", format(results[["From 2021"]][[1]][[3]], digits = 2, nsmall = 2), format(results[["From 2021"]][[2]][[3]], digits = 2, nsmall = 2), format(results[["From 2021"]][[3]][[3]], digits = 2, nsmall = 2), format(results[["From 2021"]][[4]][[3]], digits = 2, nsmall = 2), format(results[["From 2021"]][[5]][[3]], digits = 2, nsmall = 2), format(results[["From 2021"]][[6]][[3]], digits = 2, nsmall = 2)),
            c("Justeret R-squared", format(results[["From 2021"]][[1]][[4]], digits = 2, nsmall = 2), format(results[["From 2021"]][[2]][[4]], digits = 2, nsmall = 2), format(results[["From 2021"]][[3]][[4]], digits = 2, nsmall = 2), format(results[["From 2021"]][[4]][[4]], digits = 2, nsmall = 2), format(results[["From 2021"]][[5]][[4]], digits = 2, nsmall = 2), format(results[["From 2021"]][[6]][[4]], digits = 2, nsmall = 2)),
            c("Fixed effekt", "Individ", "Individ", "Individ", "Individ", "Individ", "Individ"),
            c("Standardfejl", "HAC", "HAC", "HAC", "HAC", "HAC", "HAC"),
            c("Års dummies", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja"),
            c("Årlige og ugentlige fouriertransformationer", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja")))

##### Plot resultater før og efter 2021 - ur #####

plot_data_pre_from_2021 <- list()

for (var in target_vars[1:3]) {
  for (i in 2:4) {
    # Extract the ith estimate and standard error for the variable
    estimate <- results[["From 2021"]][[var]][[1]][, 1][i]
    std_error <- results[["From 2021"]][[var]][[1]][, 2][i]
    var_name = names(results[["From 2021"]][[var]][[1]][, 2])[i]

    # Create a tibble for this specific estimate and add it to the list
    plot_data_pre_from_2021[[length(plot_data_pre_from_2021) + 1]] <- tibble(
      period = "Fra 2021",
      dep_variable = var,
      reg_variable = var_name,
      estimate = estimate,
      std_error = std_error
    )
  }
}

for (var in target_vars[1:3]) {
  for (i in 2:4) {
    # Extract the ith estimate and standard error for the variable
    estimate <- results[["Pre 2021"]][[var]][[1]][, 1][i]
    std_error <- results[["Pre 2021"]][[var]][[1]][, 2][i]
    var_name = names(results[["Pre 2021"]][[var]][[1]][, 1])[i]
    
    # Create a tibble for this specific estimate and add it to the list
    plot_data_pre_from_2021[[length(plot_data_pre_from_2021) + 1]] <- tibble(
      period = "Før 2021",
      dep_variable = var,
      reg_variable = var_name,
      estimate = estimate,
      std_error = std_error
    )
  }
}

# Dan dataframe
plot_data_pre_from_2021 <- bind_rows(plot_data_pre_from_2021)

period_shape <- c("Før 2021" = 16, "Fra 2021" = 17)

# Map reg_variable to the custom labels for the legend
plot_data_pre_from_2021$kilde <- factor(plot_data_pre_from_2021$reg_variable,
                                      levels = c("solar_penetration_hat", "offshore_penetration_hat", "onshore_penetration_hat"),
                                      labels = names_values)

dep_variables_temp <- unique(plot_data_pre_from_2021$dep_variable)
plots <- lapply(seq_along(dep_variables_temp), function(idx) {
  dep_var <- dep_variables_temp[idx]

  # Adjust the title based on the dep_variable
  title_text <- switch(dep_var,
                       "log_solar_ur" = "Sol afregningspris",
                       "log_offshore_ur" = "Havvind afregningspris",
                       "log_onshore_ur" = "Landvind afregningspris")

  # Create individual plot
  p <- ggplot(plot_data_pre_from_2021 %>% filter(dep_variable == dep_var),
              aes(x = estimate, y = kilde, color = kilde, shape = period)) +
    geom_point(position = position_dodge(width = 0.5), size = 6) +
    geom_errorbarh(aes(xmin = estimate - 1.96*std_error, xmax = estimate + 1.96*std_error),
                   height = 0.2, position = position_dodge(width = 0.5)) +
    geom_vline(xintercept = 0, linetype = "dotted", color = "black") +
    labs(title = title_text,
         x = ifelse(idx==2, "Koefficient: ln(Afregningspris) (EUR/MWh)",""),
         y = "",
         shape ="") +
    scale_color_manual(values = kilde_farve) + # Apply manual color settings
    scale_shape_manual(values = period_shape, labels = c("Før 2021", "Fra 2021")) +
    scale_x_continuous(
      limits = c(-3.3, 2),
      breaks = c(-3, -1.5, 0, 1.5), # Only show labels for -2 and 0.5
      labels = c("-3", "-1.5", "0", "1.5")) +
    guides(color = FALSE) + # Remove color legend
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 20),
          axis.title = element_text(size = 20), # Set axis titles to size 16
          axis.text.x = element_text(size = 20), # Set x-axis text to size 16
          axis.text.y = element_text(size = 20, angle = 90, vjust=0.5, hjust=0.5)) # Tilt y-axis text by 90 degrees and set size to 16

  # Apply custom y-axis labels only to the first plot
  if (idx == 1) {
    p <- p + scale_y_discrete(labels = names_values)
  } else {
    p <- p + scale_y_discrete(labels = NULL) # Keep y-axis labels for other plots
  }

  # Return the plot
  p
})

plot_pre_after_2021_ur <- wrap_plots(plots, ncol = 3) + 
  #plot_annotation(title = "", theme = theme(plot.title = element_text(size = 20, hjust = 0.5))) + 
  plot_layout(guides = 'collect') & 
  theme(legend.position = 'bottom', legend.text = element_text(size = 20))

print(plot_pre_after_2021_ur)

# Save the final plot to a file, adjusting dimensions if needed
ggsave(filename = "pre_after_2021_ur.png", plot = plot_pre_after_2021_ur , width = 16, height = 12, dpi = 300)
##### Plot resultater før og efter 2021 - CR #####

plot_data_pre_from_2021 <- list()

for (var in target_vars[4:6]) {
  for (i in 2:4) {
    # Extract the ith estimate and standard error for the variable
    estimate <- results[["From 2021"]][[var]][[1]][, 1][i]
    std_error <- results[["From 2021"]][[var]][[1]][, 2][i]
    var_name = names(results[["From 2021"]][[var]][[1]][, 2])[i]
    
    # Create a tibble for this specific estimate and add it to the list
    plot_data_pre_from_2021[[length(plot_data_pre_from_2021) + 1]] <- tibble(
      period = "Fra 2021",
      dep_variable = var,
      reg_variable = var_name,
      estimate = estimate,
      std_error = std_error
    )
  }
}

for (var in target_vars[4:6]) {
  for (i in 2:4) {
    # Extract the ith estimate and standard error for the variable
    estimate <- results[["Pre 2021"]][[var]][[1]][, 1][i]
    std_error <- results[["Pre 2021"]][[var]][[1]][, 2][i]
    var_name = names(results[["Pre 2021"]][[var]][[1]][, 1])[i]
    
    # Create a tibble for this specific estimate and add it to the list
    plot_data_pre_from_2021[[length(plot_data_pre_from_2021) + 1]] <- tibble(
      period = "Før 2021",
      dep_variable = var,
      reg_variable = var_name,
      estimate = estimate,
      std_error = std_error
    )
  }
}

# Dan dataframe
plot_data_pre_from_2021 <- bind_rows(plot_data_pre_from_2021)

period_shape <- c("Før 2021" = 16, "Fra 2021" = 17)

# Map reg_variable to the custom labels for the legend
plot_data_pre_from_2021$kilde <- factor(plot_data_pre_from_2021$reg_variable,
                                           levels = c("solar_penetration_hat", "offshore_penetration_hat", "onshore_penetration_hat"),
                                           labels = names_values)

dep_variables_temp <- unique(plot_data_pre_from_2021$dep_variable)
plots <- lapply(seq_along(dep_variables_temp), function(idx) {
  dep_var <- dep_variables_temp[idx]
  
  # Adjust the title based on the dep_variable
  title_text <- switch(dep_var,
                       "log_solar_ur" = "Sol afregningspris",
                       "log_offshore_ur" = "Havvind afregningspris",
                       "log_onshore_ur" = "Landvind afregningspris",
                       "solar_uf" = "Sol markedsværdifaktor",
                       "offshore_uf" = "Havvind markedsværdifaktor",
                       "onshore_uf" = "Landvind markedsværdifaktor")
  
  # Create individual plot
  p <- ggplot(plot_data_pre_from_2021 %>% filter(dep_variable == dep_var),
              aes(x = estimate, y = kilde, color = kilde, shape = period)) +
    geom_point(position = position_dodge(width = 0.5), size = 6) +
    geom_errorbarh(aes(xmin = estimate - 1.96*std_error, xmax = estimate + 1.96*std_error),
                   height = 0.2, position = position_dodge(width = 0.5)) +
    geom_vline(xintercept = 0, linetype = "dotted", color = "black") +
    labs(title = title_text,
         x = ifelse(idx==2, "Koefficient: Elasticiteten af afregningspris (PP)",""),
         y = "",
         shape ="") +
    scale_color_manual(values = kilde_farve) + # Apply manual color settings
    scale_shape_manual(values = period_shape, labels = c("Før 2021", "Fra 2021")) +
    scale_x_continuous(
      limits = c(-2.8, 1.8),
      breaks = c(-2.5, -1, 0, 1.5), # Only show labels for -2 and 0.5
      labels = c("-2.5", "-1", "0", "1.5")) +
    guides(color = FALSE) + # Remove color legend
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 20),
          axis.title = element_text(size = 20), # Set axis titles to size 16
          axis.text.x = element_text(size = 20), # Set x-axis text to size 16
          axis.text.y = element_text(size = 20, angle = 90, vjust=0.5, hjust=0.5)) # Tilt y-axis text by 90 degrees and set size to 16
  
  # Apply custom y-axis labels only to the first plot
  if (idx == 1) {
    p <- p + scale_y_discrete(labels = names_values)
  } else {
    p <- p + scale_y_discrete(labels = NULL) # Keep y-axis labels for other plots
  }
  
  # Return the plot
  p
})

plot_pre_after_2021_CR <- wrap_plots(plots, ncol = 3) + 
  #plot_annotation(title = "", theme = theme(plot.title = element_text(size = 20, hjust = 0.5))) + 
  plot_layout(guides = 'collect') & 
  theme(legend.position = 'bottom', legend.text = element_text(size = 20))

print(plot_pre_after_2021_CR)

# Save the final plot to a file, adjusting dimensions if needed
ggsave(filename = "pre_after_2021_CR.png", plot = plot_pre_after_2021_CR , width = 16, height = 12, dpi = 300)
##### Resultater - sommer før 2021 #####
for (var in target_vars) {
  results[["Sommer før 2021"]][[var]] <- time_series_regression(var, predictors_pre_2021, data_pre_2021_summer)
}

#print tabel
stargazer(results[["Sommer før 2021"]][[1]][[1]],results[["Sommer før 2021"]][[2]][[1]],results[["Sommer før 2021"]][[3]][[1]], results[["Sommer før 2021"]][[4]][[1]],results[["Sommer før 2021"]][[5]][[1]],results[["Sommer før 2021"]][[6]][[1]],
          title = "Fixed effect resultater for perioden 2015 til 2020 for penetrations raternes effekt på enhedspris og relative pris",
          table.placement = "H",
          label="tab:result_pre_2021_sommer",
          header = FALSE,
          type = "latex",
          column.labels = c("Sol", "Havvind", "Landvind", "Sol", "Havvind", "Landvind"),
          column.sep.width = "10pt",
          align = TRUE,
          dep.var.labels.include = FALSE,
          model.names = FALSE,
          digits=2,
          out = "firststage_regression.tex",
          #covariate.labels = predictors_second_stage <- c("Sol pen", "Havvind pen", "Landvind pen",  "Konstant"), #"$D\\_{region}$",
          #omit = c(interaction_terms_pre_2021, d_years_pre, "region_dummy", "is_weekend", "constant"),# c("fourier_h_7_cos", "fourier_h_7_sin", "fourier_h_365_cos", "fourier_h_365_sin", "is_weekend", "temperature", "D_2016","D_2017","D_2018","D_2016","D_2019","D_2020","D_2021","D_2022","D_2023"),
          covariate.labels = c("Sol pen", "Havvind pen", "Landvind pen", "Temperatur"),
          keep = vars_non_season[1:4], 
          add.lines = list(
            c("Antal observationer", format(results[["Sommer før 2021"]][[1]][[2]], digits = 2, nsmall = 2), format(results[["Sommer før 2021"]][[2]][[2]], digits = 2, nsmall = 2), format(results[["Sommer før 2021"]][[3]][[2]], digits = 2, nsmall = 2), format(results[["Sommer før 2021"]][[4]][[2]], digits = 2, nsmall = 2), format(results[["Sommer før 2021"]][[5]][[2]], digits = 2, nsmall = 2), format(results[["Sommer før 2021"]][[6]][[2]], digits = 2, nsmall = 2)),
            c("Frihedsgrader", format(as.numeric(attr(results[["Sommer før 2021"]][["pre"]][[1]][["coeftest"]], "df")), digits = 2),
              format(as.numeric(attr(results[["Sommer før 2021"]][[2]][["coeftest"]], "df")), digits = 2),
              format(as.numeric(attr(results[["Sommer før 2021"]][[3]][["coeftest"]], "df")), digits = 2),
              format(as.numeric(attr(results[["Sommer før 2021"]][[4]][["coeftest"]], "df")), digits = 2),
              format(as.numeric(attr(results[["Sommer før 2021"]][[5]][["coeftest"]], "df")), digits = 2),
              format(as.numeric(attr(results[["Sommer før 2021"]][[6]][["coeftest"]], "df")), digits = 2)),
            c("R-squared", format(results[["Sommer før 2021"]][[1]][[3]], digits = 2, nsmall = 2), format(results[["Sommer før 2021"]][[2]][[3]], digits = 2, nsmall = 2), format(results[["Sommer før 2021"]][[3]][[3]], digits = 2, nsmall = 2), format(results[["Sommer før 2021"]][[4]][[3]], digits = 2, nsmall = 2), format(results[["Sommer før 2021"]][[5]][[3]], digits = 2, nsmall = 2), format(results[["Sommer før 2021"]][[6]][[3]], digits = 2, nsmall = 2)),
            c("Justeret R-squared", format(results[["Sommer før 2021"]][[1]][[4]], digits = 2, nsmall = 2), format(results[["Sommer før 2021"]][[2]][[4]], digits = 2, nsmall = 2), format(results[["Sommer før 2021"]][[3]][[4]], digits = 2, nsmall = 2), format(results[["Sommer før 2021"]][[4]][[4]], digits = 2, nsmall = 2), format(results[["Sommer før 2021"]][[5]][[4]], digits = 2, nsmall = 2), format(results[["Sommer før 2021"]][[6]][[4]], digits = 2, nsmall = 2)),
            c("Fixed effekt", "Individ", "Individ", "Individ", "Individ", "Individ", "Individ"),
            c("Standardfejl", "HAC", "HAC", "HAC", "HAC", "HAC", "HAC"),
            c("Års dummies", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja"),
            c("Årlige og ugentlige fouriertransformationer", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja")))

##### Resultater - vinter før 2021 #####

for (var in target_vars) {
  results[["vinter før 2021"]][[var]] <- time_series_regression(var, predictors_from_2021, data_pre_2021_winter)
}

#print tabel
stargazer(results[["vinter før 2021"]][[1]][[1]],results[["vinter før 2021"]][[2]][[1]],results[["vinter før 2021"]][[3]][[1]], results[["vinter før 2021"]][[4]][[1]],results[["vinter før 2021"]][[5]][[1]],results[["vinter før 2021"]][[6]][[1]],
          title = "Fixed effect resultater for perioden 2015 til 2020 for penetrations raternes effekt på enhedspris og relative pris",
          table.placement = "H",
          label="tab:result_pre_2021_sommer",
          header = FALSE,
          type = "latex",
          column.labels = c("Sol", "Havvind", "Landvind", "Sol", "Havvind", "Landvind"),
          column.sep.width = "10pt",
          align = TRUE,
          dep.var.labels.include = FALSE,
          model.names = FALSE,
          digits=2,
          out = "firststage_regression.tex",
          #covariate.labels = predictors_second_stage <- c("Sol pen", "Havvind pen", "Landvind pen",  "Konstant"), #"$D\\_{region}$",
          #omit = c(interaction_terms_pre_2021, d_years_pre, "region_dummy", "is_weekend", "constant"),# c("fourier_h_7_cos", "fourier_h_7_sin", "fourier_h_365_cos", "fourier_h_365_sin", "is_weekend", "temperature", "D_2016","D_2017","D_2018","D_2016","D_2019","D_2020","D_2021","D_2022","D_2023"),
          covariate.labels = c("Sol pen", "Havvind pen", "Landvind pen", "Temperatur"),
          keep = vars_non_season[1:4], 
          add.lines = list(
            c("Antal observationer", format(results[["vinter før 2021"]][[1]][[2]], digits = 2, nsmall = 2), format(results[["vinter før 2021"]][[2]][[2]], digits = 2, nsmall = 2), format(results[["vinter før 2021"]][[3]][[2]], digits = 2, nsmall = 2), format(results[["vinter før 2021"]][[4]][[2]], digits = 2, nsmall = 2), format(results[["vinter før 2021"]][[5]][[2]], digits = 2, nsmall = 2), format(results[["vinter før 2021"]][[6]][[2]], digits = 2, nsmall = 2)),
            c("Frihedsgrader", format(as.numeric(attr(results[["vinter før 2021"]][["pre"]][[1]][["coeftest"]], "df")), digits = 2),
              format(as.numeric(attr(results[["vinter før 2021"]][[2]][["coeftest"]], "df")), digits = 2),
              format(as.numeric(attr(results[["vinter før 2021"]][[3]][["coeftest"]], "df")), digits = 2),
              format(as.numeric(attr(results[["vinter før 2021"]][[4]][["coeftest"]], "df")), digits = 2),
              format(as.numeric(attr(results[["vinter før 2021"]][[5]][["coeftest"]], "df")), digits = 2),
              format(as.numeric(attr(results[["vinter før 2021"]][[6]][["coeftest"]], "df")), digits = 2)),
            c("R-squared", format(results[["vinter før 2021"]][[1]][[3]], digits = 2, nsmall = 2), format(results[["vinter før 2021"]][[2]][[3]], digits = 2, nsmall = 2), format(results[["vinter før 2021"]][[3]][[3]], digits = 2, nsmall = 2), format(results[["vinter før 2021"]][[4]][[3]], digits = 2, nsmall = 2), format(results[["vinter før 2021"]][[5]][[3]], digits = 2, nsmall = 2), format(results[["vinter før 2021"]][[6]][[3]], digits = 2, nsmall = 2)),
            c("Justeret R-squared", format(results[["vinter før 2021"]][[1]][[4]], digits = 2, nsmall = 2), format(results[["vinter før 2021"]][[2]][[4]], digits = 2, nsmall = 2), format(results[["vinter før 2021"]][[3]][[4]], digits = 2, nsmall = 2), format(results[["vinter før 2021"]][[4]][[4]], digits = 2, nsmall = 2), format(results[["vinter før 2021"]][[5]][[4]], digits = 2, nsmall = 2), format(results[["vinter før 2021"]][[6]][[4]], digits = 2, nsmall = 2)),
            c("Fixed effekt", "Individ", "Individ", "Individ", "Individ", "Individ", "Individ"),
            c("Standardfejl", "HAC", "HAC", "HAC", "HAC", "HAC", "HAC"),
            c("Års dummies", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja"),
            c("Årlige og ugentlige fouriertransformationer", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja")))

##### Plot resultater sommer og vinter før 2021 - ur #####

plot_data_summer_winter_2021 <- list()

for (var in target_vars[1:3]) {
  for (i in 1:3) {
    # Extract the ith estimate and standard error for the variable
    estimate <- results[["vinter før 2021"]][[var]][[1]][, 1][i+1]
    std_error <- results[["vinter før 2021"]][[var]][[1]][, 2][i+1]
    var_name = names(results[["vinter før 2021"]][[var]][[1]][, 2])[i+1]
    
    # Create a tibble for this specific estimate and add it to the list
    plot_data_summer_winter_2021[[length(plot_data_summer_winter_2021) + 1]] <- tibble(
      period = "Vinter",
      dep_variable = var,
      reg_variable = var_name,
      estimate = estimate,
      std_error = std_error
    )
  }
}

for (var in target_vars[1:3]) {
  for (i in 1:3) {
    # Extract the ith estimate and standard error for the variable
    estimate <- results[["Sommer før 2021"]][[var]][[1]][, 1][i+1]
    std_error <- results[["Sommer før 2021"]][[var]][[1]][, 2][i+1]
    var_name = names(results[["Sommer før 2021"]][[var]][[1]][, 1])[i+1]
    
    # Create a tibble for this specific estimate and add it to the list
    plot_data_summer_winter_2021[[length(plot_data_summer_winter_2021) + 1]] <- tibble(
      period = "Sommer",
      dep_variable = var,
      reg_variable = var_name,
      estimate = estimate,
      std_error = std_error
    )
  }
}

# Dan dataframe
plot_data_summer_winter_2021 <- bind_rows(plot_data_summer_winter_2021)

period_shape <- c("Sommer" = 16, "Vinter" = 17)

# Map reg_variable to the custom labels for the legend
plot_data_summer_winter_2021$kilde <- factor(plot_data_summer_winter_2021$reg_variable,
                                           levels = c("solar_penetration_hat", "offshore_penetration_hat", "onshore_penetration_hat"),
                                           labels = names_values)

dep_variables_temp <- unique(plot_data_summer_winter_2021$dep_variable)
plots <- lapply(seq_along(dep_variables_temp), function(idx) {
  dep_var <- dep_variables_temp[idx]
  
  # Adjust the title based on the dep_variable
  title_text <- switch(dep_var,
                       "log_solar_ur" = "Sol afregningspris",
                       "log_offshore_ur" = "Havvind afregningspris",
                       "log_onshore_ur" = "Landvind afregningspris")
  
  # Create individual plot
  p <- ggplot(plot_data_summer_winter_2021 %>% filter(dep_variable == dep_var),
              aes(x = estimate, y = kilde, color = kilde, shape = period)) +
    geom_point(position = position_dodge(width = 0.5), size = 6) +
    geom_errorbarh(aes(xmin = estimate - 1.96*std_error, xmax = estimate + 1.96*std_error),
                   height = 0.2, position = position_dodge(width = 0.5)) +
    geom_vline(xintercept = 0, linetype = "dotted", color = "black") +
    labs(title = title_text,
         x = ifelse(idx==2, "Koefficient: ln(Afregningspris) (EUR/MWh)",""),
         y = "",
         shape ="") +
    scale_color_manual(values = kilde_farve) + # Apply manual color settings
    scale_shape_manual(values = period_shape, labels = c("Vinter", "Sommer")) +
    scale_x_continuous(
      limits = c(-20, 7.5),
      breaks = c(-18, -10, -2, 6), # Only show labels for -2 and 0.5
      labels = c("-18", "-10", "-2", "6")) +
    guides(color = FALSE) + # Remove color legend
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 20),
          axis.title = element_text(size = 20), # Set axis titles to size 16
          axis.text.x = element_text(size = 20), # Set x-axis text to size 16
          axis.text.y = element_text(size = 20, angle = 90, vjust=0.5, hjust=0.5)) # Tilt y-axis text by 90 degrees and set size to 16
  
  # Apply custom y-axis labels only to the first plot
  if (idx == 1) {
    p <- p + scale_y_discrete(labels = names_values)
  } else {
    p <- p + scale_y_discrete(labels = NULL) # Keep y-axis labels for other plots
  }
  
  # Return the plot
  p
})

plot_summer_winter_2021_ur <- wrap_plots(plots, ncol = 3) + 
  #plot_annotation(title = "", theme = theme(plot.title = element_text(size = 20, hjust = 0.5))) + 
  plot_layout(guides = 'collect') & 
  theme(legend.position = 'bottom', legend.text = element_text(size = 20))

print(plot_summer_winter_2021_ur)

# Save the final plot to a file, adjusting dimensions if needed
ggsave(filename = "plot_summer_winter_2021_ur.png", plot = plot_summer_winter_2021_ur , width = 16, height = 12, dpi = 300)
##### Plot resultater sommer og vinter før 2021 - CR #####

plot_data_summer_winter_2021 <- list()

for (var in target_vars[4:6]) {
  for (i in 1:3) {
    # Extract the ith estimate and standard error for the variable
    estimate <- results[["vinter før 2021"]][[var]][[1]][, 1][i+1]
    std_error <- results[["vinter før 2021"]][[var]][[1]][, 2][i+1]
    var_name = names(results[["vinter før 2021"]][[var]][[1]][, 2])[i+1]
    
    # Create a tibble for this specific estimate and add it to the list
    plot_data_summer_winter_2021[[length(plot_data_summer_winter_2021) + 1]] <- tibble(
      period = "Sommer",
      dep_variable = var,
      reg_variable = var_name,
      estimate = estimate,
      std_error = std_error
    )
  }
}

for (var in target_vars[4:6]) {
  for (i in 1:3) {
    # Extract the ith estimate and standard error for the variable
    estimate <- results[["Sommer før 2021"]][[var]][[1]][, 1][i+1]
    std_error <- results[["Sommer før 2021"]][[var]][[1]][, 2][i+1]
    var_name = names(results[["Sommer før 2021"]][[var]][[1]][, 1])[i+1]
    
    # Create a tibble for this specific estimate and add it to the list
    plot_data_summer_winter_2021[[length(plot_data_summer_winter_2021) + 1]] <- tibble(
      period = "Vinter",
      dep_variable = var,
      reg_variable = var_name,
      estimate = estimate,
      std_error = std_error
    )
  }
}

# Dan dataframe
plot_data_summer_winter_2021 <- bind_rows(plot_data_summer_winter_2021)
plot_data_summer_winter_2021$period <- factor(plot_data_summer_winter_2021$period, levels = c("Vinter", "Sommer"))

period_shape <- c("Vinter" = 16, "Sommer" = 17)

# Map reg_variable to the custom labels for the legend
plot_data_summer_winter_2021$kilde <- factor(plot_data_summer_winter_2021$reg_variable,
                                           levels = c("solar_penetration_hat", "offshore_penetration_hat", "onshore_penetration_hat"),
                                           labels = names_values)

dep_variables_temp <- unique(plot_data_summer_winter_2021$dep_variable)
plots <- lapply(seq_along(dep_variables_temp), function(idx) {
  dep_var <- dep_variables_temp[idx]
  
  # Adjust the title based on the dep_variable
  title_text <- switch(dep_var,
                       "log_solar_ur" = "Sol afregningspris",
                       "log_offshore_ur" = "Havvind afregningspris",
                       "log_onshore_ur" = "Landvind afregningspris",
                       "solar_uf" = "Sol markedsværdifaktor",
                       "offshore_uf" = "Havvind markedsværdifaktor",
                       "onshore_uf" = "Landvind markedsværdifaktor")
  
  # Create individual plot
  p <- ggplot(plot_data_summer_winter_2021 %>% filter(dep_variable == dep_var),
              aes(x = estimate, y = kilde, color = kilde, shape = period)) +
    geom_point(position = position_dodge(width = 0.5), size = 6) +
    geom_errorbarh(aes(xmin = estimate - 1.96*std_error, xmax = estimate + 1.96*std_error),
                   height = 0.2, position = position_dodge(width = 0.5)) +
    geom_vline(xintercept = 0, linetype = "dotted", color = "black") +
    labs(title = title_text,
         x = ifelse(idx==2, "Koefficient: Elasticiteten af afregningspris (PP)",""),
         y = "",
         shape ="") +
    scale_color_manual(values = kilde_farve) + # Apply manual color settings
    scale_shape_manual(values = period_shape, labels = c("Vinter", "Sommer")) +
    scale_x_continuous(
      limits = c(-8, 4.5),
      breaks = c(-6, -3, -0, 3), # Only show labels for -2 and 0.5
      labels = c("-8", "-4", "-1", "3")) +
    guides(color = FALSE) + # Remove color legend
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 20),
          axis.title = element_text(size = 20), # Set axis titles to size 16
          axis.text.x = element_text(size = 20), # Set x-axis text to size 16
          axis.text.y = element_text(size = 20, angle = 90, vjust=0.5, hjust=0.5)) # Tilt y-axis text by 90 degrees and set size to 16
  
  # Apply custom y-axis labels only to the first plot
  if (idx == 1) {
    p <- p + scale_y_discrete(labels = names_values)
  } else {
    p <- p + scale_y_discrete(labels = NULL) # Keep y-axis labels for other plots
  }
  
  # Return the plot
  p
})

plot_summer_winter_2021_CR <- wrap_plots(plots, ncol = 3) + 
  #plot_annotation(title = "", theme = theme(plot.title = element_text(size = 20, hjust = 0.5))) + 
  plot_layout(guides = 'collect') & 
  theme(legend.position = 'bottom', legend.text = element_text(size = 20))

print(plot_summer_winter_2021_CR)

# Save the final plot to a file, adjusting dimensions if needed
ggsave(filename = "plot_summer_winter_2021_CR.png", plot = plot_summer_winter_2021_CR , width = 16, height = 12, dpi = 300)

##### Regression for DK1 og DK2 - før 2021 #####

#Kør regression
result_region <- list()
for (region in c("DK_1", "DK_2")) {
  if (region == "DK_1") {
    data_temp = data_pre_2021_DK1
  } else {
    data_temp = data_pre_2021_DK2
  }
  
  for (var in target_vars) {
    results[["region"]][[region]][[var]] <- time_series_regression(var, predictors_region, data_temp)
  }
}

stargazer(results[["region"]][[1]][[1]][[1]],results[["region"]][[1]][[2]][[1]],results[["region"]][[1]][[3]][[1]], results[["region"]][[1]][[4]][[1]],results[["region"]][[1]][[5]][[1]],results[["region"]][[1]][[6]][[1]],
          title = "Fixed effect resultater for perioden 2015 til 2020 for penetrations raternes effekt på enhedspris og relative pris",
          table.placement = "H",
          label="tab:result_pre_2021_DK1",
          header = FALSE,
          type = "latex",
          column.labels = c("Sol", "Havvind", "Landvind", "Sol", "Havvind", "Landvind"),
          column.sep.width = "10pt",
          align = TRUE,
          dep.var.labels.include = FALSE,
          model.names = FALSE,
          digits=2,
          out = "firststage_regression.tex",
          #covariate.labels = predictors_second_stage <- c("Sol pen", "Havvind pen", "Landvind pen",  "Konstant"), #"$D\\_{region}$",
          #omit = c(interaction_terms_pre_2021, d_years_pre, "region_dummy", "is_weekend", "constant"),# c("fourier_h_7_cos", "fourier_h_7_sin", "fourier_h_365_cos", "fourier_h_365_sin", "is_weekend", "temperature", "D_2016","D_2017","D_2018","D_2016","D_2019","D_2020","D_2021","D_2022","D_2023"),
          covariate.labels = c("Sol pen", "Havvind pen", "Landvind pen", "Temperatur"),
          keep = vars_non_season[1:4], 
          add.lines = list(
            c("Antal observationer", format(results[["region"]][[1]][[1]][[2]], digits = 2, nsmall = 2), format(results[["region"]][[1]][[2]][[2]], digits = 2, nsmall = 2), format(results[["region"]][[1]][[3]][[2]], digits = 2, nsmall = 2), format(results[["region"]][[1]][[4]][[2]], digits = 2, nsmall = 2), format(results[["region"]][[1]][[5]][[2]], digits = 2, nsmall = 2), format(results[["region"]][[1]][[6]][[2]], digits = 2, nsmall = 2)),
            c("Frihedsgrader", format(as.numeric(attr(results[["region"]][[1]][["pre"]][[1]][["coeftest"]], "df")), digits = 2),
              format(as.numeric(attr(results[["region"]][[1]][[2]][["coeftest"]], "df")), digits = 2),
              format(as.numeric(attr(results[["region"]][[1]][[3]][["coeftest"]], "df")), digits = 2),
              format(as.numeric(attr(results[["region"]][[1]][[4]][["coeftest"]], "df")), digits = 2),
              format(as.numeric(attr(results[["region"]][[1]][[5]][["coeftest"]], "df")), digits = 2),
              format(as.numeric(attr(results[["region"]][[1]][[6]][["coeftest"]], "df")), digits = 2)),
            c("R-squared", format(results[["region"]][[1]][[1]][[3]], digits = 2, nsmall = 2), format(results[["region"]][[1]][[2]][[3]], digits = 2, nsmall = 2), format(results[["region"]][[1]][[3]][[3]], digits = 2, nsmall = 2), format(results[["region"]][[1]][[4]][[3]], digits = 2, nsmall = 2), format(results[["region"]][[1]][[5]][[3]], digits = 2, nsmall = 2), format(results[["region"]][[1]][[6]][[3]], digits = 2, nsmall = 2)),
            c("Justeret R-squared", format(results[["region"]][[1]][[1]][[4]], digits = 2, nsmall = 2), format(results[["region"]][[1]][[2]][[4]], digits = 2, nsmall = 2), format(results[["region"]][[1]][[3]][[4]], digits = 2, nsmall = 2), format(results[["region"]][[1]][[4]][[4]], digits = 2, nsmall = 2), format(results[["region"]][[1]][[5]][[4]], digits = 2, nsmall = 2), format(results[["region"]][[1]][[6]][[4]], digits = 2, nsmall = 2)),
            c("Fixed effekt", "Individ", "Individ", "Individ", "Individ", "Individ", "Individ"),
            c("Standardfejl", "HAC", "HAC", "HAC", "HAC", "HAC", "HAC"),
            c("Års dummies", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja"),
            c("Årlige og ugentlige fouriertransformationer", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja")))


stargazer(results[["region"]][[2]][[1]][[1]],results[["region"]][[2]][[2]][[1]],results[["region"]][[2]][[3]][[1]], results[["region"]][[2]][[4]][[1]],results[["region"]][[2]][[5]][[1]],results[["region"]][[2]][[6]][[1]],
          title = "Fixed effect resultater for perioden 2015 til 2020 for penetrations raternes effekt på enhedspris og relative pris",
          table.placement = "H",
          label="tab:result_pre_2021_DK1",
          header = FALSE,
          type = "latex",
          column.labels = c("Sol", "Havvind", "Landvind", "Sol", "Havvind", "Landvind"),
          column.sep.width = "10pt",
          align = TRUE,
          dep.var.labels.include = FALSE,
          model.names = FALSE,
          digits=2,
          out = "firststage_regression.tex",
          #covariate.labels = predictors_second_stage <- c("Sol pen", "Havvind pen", "Landvind pen",  "Konstant"), #"$D\\_{region}$",
          #omit = c(interaction_terms_pre_2021, d_years_pre, "region_dummy", "is_weekend", "constant"),# c("fourier_h_7_cos", "fourier_h_7_sin", "fourier_h_365_cos", "fourier_h_365_sin", "is_weekend", "temperature", "D_2016","D_2017","D_2018","D_2016","D_2019","D_2020","D_2021","D_2022","D_2023"),
          covariate.labels = c("Sol pen", "Havvind pen", "Landvind pen", "Temperatur"),
          keep = vars_non_season[1:4], 
          add.lines = list(
            c("Antal observationer", format(results[["region"]][[2]][[1]][[2]], digits = 2, nsmall = 2), format(results[["region"]][[2]][[2]][[2]], digits = 2, nsmall = 2), format(results[["region"]][[2]][[3]][[2]], digits = 2, nsmall = 2), format(results[["region"]][[2]][[4]][[2]], digits = 2, nsmall = 2), format(results[["region"]][[2]][[5]][[2]], digits = 2, nsmall = 2), format(results[["region"]][[2]][[6]][[2]], digits = 2, nsmall = 2)),
            c("Frihedsgrader", format(as.numeric(attr(results[["region"]][[2]][["pre"]][[1]][["coeftest"]], "df")), digits = 2),
              format(as.numeric(attr(results[["region"]][[2]][[2]][["coeftest"]], "df")), digits = 2),
              format(as.numeric(attr(results[["region"]][[2]][[3]][["coeftest"]], "df")), digits = 2),
              format(as.numeric(attr(results[["region"]][[2]][[4]][["coeftest"]], "df")), digits = 2),
              format(as.numeric(attr(results[["region"]][[2]][[5]][["coeftest"]], "df")), digits = 2),
              format(as.numeric(attr(results[["region"]][[2]][[6]][["coeftest"]], "df")), digits = 2)),
            c("R-squared", format(results[["region"]][[2]][[1]][[3]], digits = 2, nsmall = 2), format(results[["region"]][[2]][[2]][[3]], digits = 2, nsmall = 2), format(results[["region"]][[2]][[3]][[3]], digits = 2, nsmall = 2), format(results[["region"]][[2]][[4]][[3]], digits = 2, nsmall = 2), format(results[["region"]][[2]][[5]][[3]], digits = 2, nsmall = 2), format(results[["region"]][[2]][[6]][[3]], digits = 2, nsmall = 2)),
            c("Justeret R-squared", format(results[["region"]][[2]][[1]][[4]], digits = 2, nsmall = 2), format(results[["region"]][[2]][[2]][[4]], digits = 2, nsmall = 2), format(results[["region"]][[2]][[3]][[4]], digits = 2, nsmall = 2), format(results[["region"]][[2]][[4]][[4]], digits = 2, nsmall = 2), format(results[["region"]][[2]][[5]][[4]], digits = 2, nsmall = 2), format(results[["region"]][[2]][[6]][[4]], digits = 2, nsmall = 2)),
            c("Fixed effekt", "Individ", "Individ", "Individ", "Individ", "Individ", "Individ"),
            c("Standardfejl", "HAC", "HAC", "HAC", "HAC", "HAC", "HAC"),
            c("Års dummies", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja"),
            c("Årlige og ugentlige fouriertransformationer", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja")))

##### Plot for DK1 og DK2 - før 2021 UR #####
#liste til plotdata
plot_data_region <- list()
for (region in c("DK_1", "DK_2")) {
  for (var in target_vars[1:3]) {
    for (i in 2:4) {
      # Extract the ith estimate and standard error for the variable
      estimate <- results[["region"]][[region]][[var]][["coeftest"]][, 1][i]
      std_error <- results[["region"]][[region]][[var]][["coeftest"]][, 2][i]
      var_name = names(results[["region"]][[region]][[var]][["coeftest"]][, 1])[i]
      
      # Create a tibble for this specific estimate and add it to the list
      plot_data_region[[length(plot_data_region) + 1]] <- tibble(
        region = region,
        dep_variable = var,
        reg_variable = var_name,
        estimate = estimate,
        std_error = std_error
      )
    }
  }
}

# Combine all the tibbles in the list into one dataframe
plot_data_region <- bind_rows(plot_data_region)

dk_shape <- c("DK_1" = 16, "DK_2" = 17)

# Map reg_variable to the custom labels for the legend
plot_data_region$kilde <- factor(plot_data_region$reg_variable,
                                       levels = c("solar_penetration_hat", "offshore_penetration_hat", "onshore_penetration_hat"),
                                       labels = names_values)

dep_variables_temp <- unique(plot_data_region$dep_variable)
plots <- lapply(seq_along(dep_variables_temp), function(idx) {
  dep_var <- dep_variables_temp[idx]
  
  # Adjust the title based on the dep_variable
  title_text <- switch(dep_var,
                       "log_solar_ur" = "Sol afregningspris",
                       "log_offshore_ur" = "Havvind afregningspris",
                       "log_onshore_ur" = "Landvind afregningspris")
  
  # Create individual plot
  p <- ggplot(plot_data_region %>% filter(dep_variable == dep_var), 
              aes(x = estimate, y = kilde, color = kilde, shape = region)) +
    geom_point(position = position_dodge(width = 0.5), size = 6) +
    geom_errorbarh(aes(xmin = estimate - 1.96*std_error, xmax = estimate + 1.96*std_error),
                   height = 0.2, position = position_dodge(width = 0.5)) +
    geom_vline(xintercept = 0, linetype = "dotted", color = "black") +
    labs(title = title_text,
         x = ifelse(idx==2, "Koefficient: ln(Afregningspris) (EUR/MWh)",""),
         y = "",
         shape ="") +
    scale_color_manual(values = kilde_farve) + # Apply manual color settings
    scale_shape_manual(values = dk_shape, labels = c("DK 1", "DK 2")) +
    scale_x_continuous(
      limits = c(-4, 2.5),
      breaks = c(-4, -2, 0, 2), # Only show labels for -2 and 0.5
      labels = c("-4", "-2","0", "2")) +
    guides(color = FALSE) + # Remove color legend
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 20),
          axis.title = element_text(size = 20), # Set axis titles to size 16
          axis.text.x = element_text(size = 20), # Set x-axis text to size 16
          axis.text.y = element_text(size = 20, angle = 90, vjust=0.5, hjust=0.5)) # Tilt y-axis text by 90 degrees and set size to 16
  
  # Apply custom y-axis labels only to the first plot
  if (idx == 1) {
    p <- p + scale_y_discrete(labels = names_values)
  } else {
    p <- p + scale_y_discrete(labels = NULL) # Keep y-axis labels for other plots
  }
  
  # Return the plot
  p
})

DK1_DK2_plot <- wrap_plots(plots, ncol = 3) + 
  #plot_annotation(title = "", theme = theme(plot.title = element_text(size = 20, hjust = 0.5))) + 
  plot_layout(guides = 'collect') & 
  theme(legend.position = 'bottom', legend.text = element_text(size = 20))

print(DK1_DK2_plot)

# Save the final plot to a file, adjusting dimensions if needed
ggsave(filename = "DK1_DK2_plot_ur.png", plot = DK1_DK2_plot, width = 16, height = 12, dpi = 300)

##### Plot for DK1 og DK2 - før 2021 CR #####
#liste til plotdata
plot_data_region <- list()
for (region in c("DK_1", "DK_2")) {
  for (var in target_vars[4:6]) {
    for (i in 2:4) {
      # Extract the ith estimate and standard error for the variable
      estimate <- results[["region"]][[region]][[var]][["coeftest"]][, 1][i]
      std_error <- results[["region"]][[region]][[var]][["coeftest"]][, 2][i]
      var_name = names(results[["region"]][[region]][[var]][["coeftest"]][, 1])[i]
      
      # Create a tibble for this specific estimate and add it to the list
      plot_data_region[[length(plot_data_region) + 1]] <- tibble(
        region = region,
        dep_variable = var,
        reg_variable = var_name,
        estimate = estimate,
        std_error = std_error
      )
    }
  }
}

# Combine all the tibbles in the list into one dataframe
plot_data_region <- bind_rows(plot_data_region)

dk_shape <- c("DK_1" = 16, "DK_2" = 17)

# Map reg_variable to the custom labels for the legend
plot_data_region$kilde <- factor(plot_data_region$reg_variable,
                                 levels = c("solar_penetration_hat", "offshore_penetration_hat", "onshore_penetration_hat"),
                                 labels = names_values)

dep_variables_temp <- unique(plot_data_region$dep_variable)
plots <- lapply(seq_along(dep_variables_temp), function(idx) {
  dep_var <- dep_variables_temp[idx]
  
  # Adjust the title based on the dep_variable
  title_text <- switch(dep_var,
                       "log_solar_ur" = "Sol afregningspris",
                       "log_offshore_ur" = "Havvind afregningspris",
                       "log_onshore_ur" = "Landvind afregningspris",
                       "solar_uf" = "Sol markedsværdifaktor",
                       "offshore_uf" = "Havvind markedsværdifaktor",
                       "onshore_uf" = "Landvind markedsværdifaktor")
  
  # Create individual plot
  p <- ggplot(plot_data_region %>% filter(dep_variable == dep_var), 
              aes(x = estimate, y = kilde, color = kilde, shape = region)) +
    geom_point(position = position_dodge(width = 0.5), size = 6) +
    geom_errorbarh(aes(xmin = estimate - 1.96*std_error, xmax = estimate + 1.96*std_error),
                   height = 0.2, position = position_dodge(width = 0.5)) +
    geom_vline(xintercept = 0, linetype = "dotted", color = "black") +
    labs(title = title_text,
         x = ifelse(idx==2, "Koefficient: Elasticiteten af afregningspris (PP)",""),
         y = "",
         shape ="") +
    scale_color_manual(values = kilde_farve) + # Apply manual color settings
    scale_shape_manual(values = dk_shape, labels = c("DK 1", "DK 2")) +
    scale_x_continuous(
      limits = c(-3, 1.5),
      breaks = c(-3, -1, 1), # Only show labels for -2 and 0.5
      labels = c("-3","-1", "1")) +
    guides(color = FALSE) + # Remove color legend
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 20),
          axis.title = element_text(size = 20), # Set axis titles to size 16
          axis.text.x = element_text(size = 20), # Set x-axis text to size 16
          axis.text.y = element_text(size = 20, angle = 90, vjust=0.5, hjust=0.5)) # Tilt y-axis text by 90 degrees and set size to 16
  
  # Apply custom y-axis labels only to the first plot
  if (idx == 1) {
    p <- p + scale_y_discrete(labels = names_values)
  } else {
    p <- p + scale_y_discrete(labels = NULL) # Keep y-axis labels for other plots
  }
  
  # Return the plot
  p
})

DK1_DK2_plot_CR <- wrap_plots(plots, ncol = 3) + 
  #plot_annotation(title = "", theme = theme(plot.title = element_text(size = 20, hjust = 0.5))) + 
  plot_layout(guides = 'collect') & 
  theme(legend.position = 'bottom', legend.text = element_text(size = 20))

print(DK1_DK2_plot_CR)

# Save the final plot to a file, adjusting dimensions if needed
ggsave(filename = "DK1_DK2_plot_CR.png", plot = DK1_DK2_plot_CR, width = 16, height = 12, dpi = 300)

##### Plot af kommulativit niveau for penetration - før 2021 opdelt på DK1 og DK2#####

cum_data_DK1_DK2 <- data.frame()
# Apply the function to each variable for each region
for (reg in c("DK_1", "DK_2")) {
  # Filter data for the region
  data_temp <- subset(data_pre_2021, region == reg)
  
  # Calculate cumulative counts for each variable and add to cum_data_DK1_DK2
  cum_data_DK1_DK2 <- rbind(cum_data_DK1_DK2,
                            transform(cumulative_count(data_temp$solar_penetration_hat), variable = "solar_penetration_hat", region = reg),
                            transform(cumulative_count(data_temp$offshore_penetration_hat), variable = "offshore_penetration_hat", region = reg),
                            transform(cumulative_count(data_temp$onshore_penetration_hat), variable = "onshore_penetration_hat", region = reg)
  )
}

split_points <- data.frame()

for (region in c("DK_1", "DK_2")){
  for (var in unique(cum_data_DK1_DK2$variable)) {
    region = region
    var_data <- cum_data_DK1_DK2[cum_data_DK1_DK2$variable == var & cum_data_DK1_DK2$region == region,]
    total <- max(var_data$cumcount)
    one_third_y = total/3
    one_third_x <- var_data$value[which.min(abs(var_data$cumcount - one_third_y))]
    two_thirds_y = 2*total/3
    two_thirds_x <- var_data$value[which.min(abs(var_data$cumcount - two_thirds_y))]
    
    
    split_points <- rbind(split_points, data.frame(variable = var, one_third_x = one_third_x, one_third_y = one_third_y, two_thirds_x = two_thirds_x, two_thirds_y = two_thirds_y, region = region))
  }
}
cum_data_DK1_DK2$variable <- factor(cum_data_DK1_DK2$variable, 
                                    levels = c("solar_penetration_hat", "offshore_penetration_hat", "onshore_penetration_hat"))

cum_DK1_DK2 <- ggplot(cum_data_DK1_DK2, aes(x = value, y = cumcount, color = variable)) +
  geom_line(aes(linetype = region), linewidth = 2) + 
  scale_color_manual(
    values = c("solar_penetration_hat" = "orange", "offshore_penetration_hat" = "blue", "onshore_penetration_hat" = "brown"),
    labels = c("Sol pen", "Havvind", "Landvind")
  ) +
  geom_point(data = split_points, aes(x = one_third_x, y = one_third_y), color = "black", size = 3) +
  geom_point(data = split_points, aes(x = two_thirds_x, y = two_thirds_y), color = "black", size = 3) +
  
  labs(
    title = "",
    x = "Penetrationsrate (%)",
    y = "Kummulativt antal"
  ) +
  scale_linetype_manual(
    values = c("DK_1" = "solid", "DK_2" = "dotted"),
    labels = c("DK 1", "DK 2")
  ) + 
  theme_minimal() +
  theme(
    legend.position = c(0.8, 0.1),
    legend.box = "horizontal",
    legend.text = element_text(size = 16),       # Set legend text size
    axis.title = element_text(size = 16),         # Set axis title size
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    plot.background = element_rect(fill = "white", colour = "white")
  ) +
  guides(
    color = guide_legend(title = NULL),
    linetype = guide_legend(title = NULL)
  ) +
  geom_blank(aes(color = variable, linetype = region))
ggsave("cum_DK1_DK2.png", plot = cum_DK1_DK2, width = 10, height = 8, dpi = 300)

##### Regression opdelt på niveau af pen - før 2021 opdelt på DK1 og DK2#####

# Loop through each variable and run the regression
result_penetration <- list()  # Initialize list to store regression models
# har ændret data her
for (region in c("DK_1", "DK_2")) {
  for (var_pen in c("solar_penetration_hat", "offshore_penetration_hat", "onshore_penetration_hat")) {
    one_third <- split_points$one_third_x[split_points$variable == var_pen & split_points$region == region]
    two_thirds <- split_points$two_thirds_x[split_points$variable == var_pen & split_points$region == region]
    
    # Subset the data into three parts based on split points
    
    for (i in 1:3) {
      if (i == 1) {
        data_subset <- data_pre_2021[data_pre_2021[[var_pen]] < one_third & data_pre_2021$region == region,]
      } else if (i == 2) {
        data_subset <- data_pre_2021[data_pre_2021[[var_pen]] >= one_third & data_pre_2021[[var_pen]] <= two_thirds & data_pre_2021$region == region,]
      } else {
        data_subset <- data_pre_2021[data_pre_2021[[var_pen]] > two_thirds & data_pre_2021$region == region,]
      }
      
      for (target in target_vars){
        # Run the regression for the current subset
        regression_model <- time_series_regression(target, predictors_pre_2021, data_subset)
        result_penetration[[region]][[var_pen]][[paste(i, "third")]][[target]] <- regression_model}
    }
  }
}


##### Plot pen niveau - ur DK1 #####
#samling af data til plot
plot_data_pen <- list()

for (i in seq_along(target_vars)[1:3]) { #target var [1:3] angiver ur
  var <- target_vars[i]
  for (j in 1:3) { #penetrations niveau
      # Extract the ith estimate and standard error for the variable
      estimate <- result_penetration[[1]][[i]][[j]][[i]][["coeftest"]][, 1][i+1] #var_index+1
      std_error <- result_penetration[[1]][[i]][[j]][[i]][["coeftest"]][, 2][i+1]
      var_name = names(result_penetration[[1]][[i]][[j]][[i]][["coeftest"]][, 1])[i+1]
  
    # Create a tibble for this specific estimate and add it to the list
    plot_data_pen[[length(plot_data_pen) + 1]] <- tibble(
      dep_variable = var,
      reg_variable = var_name,
      estimate = estimate,
      std_error = std_error,
      third = j
    )
  } 
}

plot_data_pen <- bind_rows(plot_data_pen)

# Calculate the confidence intervals
plot_data_pen <- plot_data_pen %>%
  mutate(
    upper_ci = estimate + (1.96 * std_error),
    lower_ci = estimate - (1.96 * std_error)
  )

# Map reg_variable to the custom labels for the legend
plot_data_pen$kilde <- factor(plot_data_pen$reg_variable,
                              levels = c("solar_penetration_hat", "offshore_penetration_hat", "onshore_penetration_hat"),
                              labels = c("Sol pen", "Havvind pen", "Landvind pen"))

dep_variables_temp <- unique(plot_data_pen$dep_variable)
plots <- lapply(seq_along(dep_variables_temp), function(idx) {
  dep_var <- dep_variables_temp[idx]
  
  # Adjust the title based on the dep_variable
  title_text <- switch(dep_var,
                       "log_solar_ur" = "Sol afregningspris",
                       "log_offshore_ur" = "Havvind afregningspris",
                       "log_onshore_ur" = "Landvind afregningspris",
                       "solar_uf" = "Sol relativt til gns. pris",
                       "offshore_uf" = "Havvind relativt til gns. pris",
                       "onshore_uf" = "Landvind relativt til gns. pris",
                       dep_var)  # Default case if none of the above
  
  one_third <- split_points[idx, "one_third_x"]
  two_thirds <- split_points[idx, "two_thirds_x"]
  
  # Creating the plot for the current dep_var
  p <- ggplot(plot_data_pen %>% filter(dep_variable == dep_var), 
              aes(x = third, group = kilde, fill = kilde, color = kilde)) +
    geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), color = NA, alpha = 0.1) +
    geom_line(aes(y = estimate), linetype = "dashed") + 
    scale_fill_manual(values = kilde_farve) +
    scale_color_manual(values = kilde_farve) +
    scale_x_continuous(
      limits = c(1, 3),
      breaks = c(1, 2, 3),
      labels = c(paste("<", sprintf("%.1f", one_third), "%"), 
                 paste("[", sprintf("%.1f", one_third), "%;", sprintf("%.1f", two_thirds), "%]", sep = ""), 
                 paste(">", sprintf("%.1f", two_thirds), "%"))
    ) +
    scale_y_continuous(limits = c(-28, 5)) + 
    theme_minimal() +
    theme(axis.text.x = element_text(size = 20),
          axis.text.y = element_text(size = 20))  # Common theme settings
  
  p <- p + labs(title = title_text, 
                x = if (idx == 1) "Sol penetration niveau" else if (idx == 2) "Havvind penetration niveau" else "Landvind penetration niveau", 
                y = if (idx == 1) "Koefficient: ln(Afregningspris) (EUR/MWh)" else "")
  
  # Setting the title text size
  p <- p + theme(plot.title = element_text(size = 20),
                 axis.title.x = element_text(size = 20),
                 axis.title.y = element_text(size = 20))
  
  p <- p + theme(legend.position = "bottom", 
                 legend.title = element_blank(),
                 legend.text = element_text(size = 20))  # Setting legend text size
  
  # if (idx == 2) {
  #   p <- p + theme(legend.position = "bottom", 
  #                  legend.title = element_blank(),
  #                  legend.text = element_text(size = 20))  # Setting legend text size
  # } else {
  #   p <- p + theme(legend.position = "none")  # Hides the legend for plots where idx is not 2
  # }
  
  return(p)  # Return the plot for each dep_variable
})

# Combine all the plots into a single object using patchwork
penetration_plot_DK1_ur <- patchwork::wrap_plots(plots, ncol = 3)
print(penetration_plot_DK1_ur)
# Save the final plot to a file
ggsave(filename = "penetration_plot_DK1_ur.png", plot = penetration_plot_DK1_ur, width = 16, height = 12, dpi = 300)

##### Plot pen niveau - CR DK1 #####
#samling af data til plot
plot_data_pen <- list()

# for (var in target_vars[4:6]) { #target var [1:3] angiver ur
#   for (i in 1:3) { #den variabel vi intresseret i som uafhængig variabel
#     for (j in 1:3) { #penetrations niveauet
#       # Extract the ith estimate and standard error for the variable
#       estimate <- result_penetration[[1]][[i]][[j]][[var]][["coeftest"]][, 1][i+1] #var_index+1
#       std_error <- result_penetration[[1]][[i]][[j]][[var]][["coeftest"]][, 2][i+1]
#       var_name = names(result_penetration[[1]][[i]][[j]][[var]][["coeftest"]][, 1])[i+1]

for (i in seq_along(target_vars)[4:6]) { #target var [1:3] angiver ur
  var <- target_vars[i]
  for (j in 1:3) { #penetrations niveau
    # Extract the ith estimate and standard error for the variable
    estimate <- result_penetration[[1]][[i-3]][[j]][[i]][["coeftest"]][, 1][i-2] #var_index-2 for at få variabel jeg er intresseret i
    std_error <- result_penetration[[1]][[i-3]][[j]][[i]][["coeftest"]][, 2][i-2]
    var_name = names(result_penetration[[1]][[i-3]][[j]][[i]][["coeftest"]][, 1])[i-2]
      
    # Create a tibble for this specific estimate and add it to the list
    plot_data_pen[[length(plot_data_pen) + 1]] <- tibble(
      dep_variable = var,
      reg_variable = var_name,
      estimate = estimate,
      std_error = std_error,
      third = j
      )
  }
}

plot_data_pen <- bind_rows(plot_data_pen)

# Calculate the confidence intervals
plot_data_pen <- plot_data_pen %>%
  mutate(
    upper_ci = estimate + (1.96 * std_error),
    lower_ci = estimate - (1.96 * std_error)
  )

# Define colors for each penetration type

# Map reg_variable to the custom labels for the legend
plot_data_pen$kilde <- factor(plot_data_pen$reg_variable,
                              levels = c("solar_penetration_hat", "offshore_penetration_hat", "onshore_penetration_hat"),
                              labels = c("Sol pen", "Havvind pen", "Landvind pen"))

dep_variables_temp <- unique(plot_data_pen$dep_variable)
plots <- lapply(seq_along(dep_variables_temp), function(idx) {
  dep_var <- dep_variables_temp[idx]
  
  # Adjust the title based on the dep_variable
  title_text <- switch(dep_var,
                       "log_solar_ur" = "Sol afregningspris",
                       "log_offshore_ur" = "Havvind afregningspris",
                       "log_onshore_ur" = "Landvind afregningspris",
                       "solar_uf" = "Sol relativt til gns. pris",
                       "offshore_uf" = "Havvind relativt til gns. pris",
                       "onshore_uf" = "Landvind relativt til gns. pris",
                       dep_var)  # Default case if none of the above
  
  one_third <- split_points[idx, "one_third_x"]
  two_thirds <- split_points[idx, "two_thirds_x"]
  
  # Creating the plot for the current dep_var
  p <- ggplot(plot_data_pen %>% filter(dep_variable == dep_var), 
              aes(x = third, group = kilde, fill = kilde, color = kilde)) +
    geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), color = NA, alpha = 0.1) +
    geom_line(aes(y = estimate), linetype = "dashed") + 
    scale_fill_manual(values = kilde_farve) +
    scale_color_manual(values = kilde_farve) +
    scale_x_continuous(
      limits = c(1, 3),
      breaks = c(1, 2, 3),
      labels = c(paste("<", sprintf("%.1f", one_third), "%"), 
                 paste("[", sprintf("%.1f", one_third), "%;", sprintf("%.1f", two_thirds), "%]", sep = ""), 
                 paste(">", sprintf("%.1f", two_thirds), "%"))
    ) +
    scale_y_continuous(limits = c(-12, 16)) +  
    labs(title = title_text) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 20),
          axis.text.y = element_text(size = 20))  # Common theme settings
  
  p <- p + labs(title = title_text, 
                x = if (idx == 1) "Sol penetration niveau" else if (idx == 2) "Havvind penetration niveau" else "Landvind penetration niveau", 
                y = if (idx == 1) "Koefficient: Elasticiteten af afregningspris (PP)" else "")
  
  # Setting the title text size
  p <- p + theme(plot.title = element_text(size = 20),
                 axis.title.x = element_text(size = 20),
                 axis.title.y = element_text(size = 20))
  
  p <- p + theme(legend.position = "bottom", 
                 legend.title = element_blank(),
                 legend.text = element_text(size = 20))  # Setting legend text size
  
  # # Conditionally apply additional theme settings
  # if (idx == 2) {
  #   p <- p + theme(legend.position = "bottom", 
  #                  legend.title = element_blank(),
  #                  legend.text = element_text(size = 20))  # Setting legend text size
  # } else {
  #   p <- p + theme(legend.position = "none")  # Hides the legend for plots where idx is not 2
  # }
  return(p)  # Return the plot for each dep_variable
})

# Combine all the plots into a single object using patchwork
penetration_plot_DK1_CR <- patchwork::wrap_plots(plots, ncol = 3)
print(penetration_plot_DK1_CR)
# Save the final plot to a file
ggsave(filename = "penetration_plot_DK1_CR.png", plot = penetration_plot_DK1_CR, width = 16, height = 12, dpi = 300)

##### Plot pen niveau - ur DK2 #####
#samling af data til plot
plot_data_pen <- list()

# for (var in target_vars[1:3]) { #target var [1:3] angiver ur
#   for (i in 1:3) { #den variabel vi intresseret i som uafhængig variabel
#     for (j in 1:3) { #penetrations niveauet
#       # Extract the ith estimate and standard error for the variable
#       estimate <- result_penetration[[2]][[i]][[j]][[var]][["coeftest"]][, 1][i+1] #var_index+1
#       std_error <- result_penetration[[2]][[i]][[j]][[var]][["coeftest"]][, 2][i+1]
#       var_name = names(result_penetration[[2]][[i]][[j]][[var]][["coeftest"]][, 1])[i+1]
#       
#       # Create a tibble for this specific estimate and add it to the list
#       plot_data_pen[[length(plot_data_pen) + 1]] <- tibble(
#         dep_variable = var,
#         reg_variable = var_name,
#         estimate = estimate,
#         std_error = std_error,
#         third = j
#       )
#     }
#   }
# }

for (i in seq_along(target_vars)[1:3]) { #target var [1:3] angiver ur
  var <- target_vars[i]
  for (j in 1:3) { #penetrations niveau
    # Extract the ith estimate and standard error for the variable
    estimate <- result_penetration[[2]][[i]][[j]][[i]][["coeftest"]][, 1][i+1] #var_index+1
    std_error <- result_penetration[[2]][[i]][[j]][[i]][["coeftest"]][, 2][i+1]
    var_name = names(result_penetration[[2]][[i]][[j]][[i]][["coeftest"]][, 1])[i+1]
    
    # Create a tibble for this specific estimate and add it to the list
    plot_data_pen[[length(plot_data_pen) + 1]] <- tibble(
      dep_variable = var,
      reg_variable = var_name,
      estimate = estimate,
      std_error = std_error,
      third = j
    )
  }
}

plot_data_pen <- bind_rows(plot_data_pen)

# Calculate the confidence intervals
plot_data_pen <- plot_data_pen %>%
  mutate(
    upper_ci = estimate + (1.96 * std_error),
    lower_ci = estimate - (1.96 * std_error)
  )

#plot_data_pen$color_edge <- c("orange", "orange","orange",NA, NA, NA,NA, NA, NA, NA, NA, NA,"blue","blue","blue", NA, NA, NA, NA, NA, NA,NA, NA, NA,"brown", "brown", "brown")

# Define colors for each penetration type

# Map reg_variable to the custom labels for the legend
plot_data_pen$kilde <- factor(plot_data_pen$reg_variable,
                              levels = c("solar_penetration_hat", "offshore_penetration_hat", "onshore_penetration_hat"),
                              labels = c("Sol pen", "Havvind pen", "Landvind pen"))

dep_variables_temp <- unique(plot_data_pen$dep_variable)
plots <- lapply(seq_along(dep_variables_temp), function(idx) {
  dep_var <- dep_variables_temp[idx]
  
  # Adjust the title based on the dep_variable
  title_text <- switch(dep_var,
                       "log_solar_ur" = "Sol afregningspris",
                       "log_offshore_ur" = "Havvind afregningspris",
                       "log_onshore_ur" = "Landvind afregningspris",
                       "solar_uf" = "Sol relativt til gns. pris",
                       "offshore_uf" = "Havvind relativt til gns. pris",
                       "onshore_uf" = "Landvind relativt til gns. pris",
                       dep_var)  # Default case if none of the above
  
  one_third <- split_points[idx+3, "one_third_x"]
  two_thirds <- split_points[idx+3, "two_thirds_x"]
  
  # Creating the plot for the current dep_var
  p <- ggplot(plot_data_pen %>% filter(dep_variable == dep_var), 
              aes(x = third, group = kilde, fill = kilde, color = kilde)) +
    geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), color = NA, alpha = 0.1) +
    geom_line(aes(y = estimate), linetype = "dashed") + 
    scale_fill_manual(values = kilde_farve) +
    scale_color_manual(values = kilde_farve) +
    scale_x_continuous(
      limits = c(1, 3),
      breaks = c(1, 2, 3),
      labels = c(paste("<", sprintf("%.1f", one_third), "%"), 
                 paste("[", sprintf("%.1f", one_third), "%;", sprintf("%.1f", two_thirds), "%]", sep = ""), 
                 paste(">", sprintf("%.1f", two_thirds), "%"))
    ) +
    scale_y_continuous(limits = c(-28, 5)) + 
    theme_minimal() +
    theme(axis.text.x = element_text(size = 20),
          axis.text.y = element_text(size = 20))  # Common theme settings
  
  p <- p + labs(title = title_text, 
                x = if (idx == 1) "Sol penetration niveau" else if (idx == 2) "Havvind penetration niveau" else "Landvind penetration niveau", 
                y = if (idx == 1) "Koefficient: ln(Afregningspris) (EUR/MWh)" else "")
  
  # Setting the title text size
  p <- p + theme(plot.title = element_text(size = 20),
                 axis.title.x = element_text(size = 20),
                 axis.title.y = element_text(size = 20))
  
  p <- p + theme(legend.position = "bottom", 
                 legend.title = element_blank(),
                 legend.text = element_text(size = 20))  # Setting legend text size
  
  # if (idx == 2) {
  #   p <- p + theme(legend.position = "bottom", 
  #                  legend.title = element_blank(),
  #                  legend.text = element_text(size = 20))  # Setting legend text size
  # } else {
  #   p <- p + theme(legend.position = "none")  # Hides the legend for plots where idx is not 2
  # }
  # 
  return(p)  # Return the plot for each dep_variable
})

# Combine all the plots into a single object using patchwork
penetration_plot_DK2_ur <- patchwork::wrap_plots(plots, ncol = 3)
print(penetration_plot_DK2_ur)
# Save the final plot to a file
ggsave(filename = "penetration_plot_DK2_ur.png", plot = penetration_plot_DK2_ur, width = 16, height = 12, dpi = 300)

##### Plot pen niveau - CR DK2 #####
plot_data_pen <- list()

# for (var in target_vars[4:6]) { #target var [1:3] angiver ur
#   for (i in 1:3) { #den variabel vi intresseret i som uafhængig variabel
#     for (j in 1:3) { #penetrations niveauet
#       # Extract the ith estimate and standard error for the variable
#       estimate <- result_penetration[[2]][[i]][[j]][[var]][["coeftest"]][, 1][i+1] #var_index+1
#       std_error <- result_penetration[[2]][[i]][[j]][[var]][["coeftest"]][, 2][i+1]
#       var_name = names(result_penetration[[2]][[i]][[j]][[var]][["coeftest"]][, 1])[i+1]
#       
#       # Create a tibble for this specific estimate and add it to the list
#       plot_data_pen[[length(plot_data_pen) + 1]] <- tibble(
#         dep_variable = var,
#         reg_variable = var_name,
#         estimate = estimate,
#         std_error = std_error,
#         third = j
#       )
#     }
#   }
# }

for (i in seq_along(target_vars)[4:6]) { #target var [1:3] angiver ur
  var <- target_vars[i]
  for (j in 1:3) { #penetrations niveau
    # Extract the ith estimate and standard error for the variable
    estimate <- result_penetration[[2]][[i-3]][[j]][[i]][["coeftest"]][, 1][i-2] #var_index+1
    std_error <- result_penetration[[2]][[i-3]][[j]][[i]][["coeftest"]][, 2][i-2]
    var_name = names(result_penetration[[2]][[i-3]][[j]][[i]][["coeftest"]][, 1])[i-2]
    
    # Create a tibble for this specific estimate and add it to the list
    plot_data_pen[[length(plot_data_pen) + 1]] <- tibble(
      dep_variable = var,
      reg_variable = var_name,
      estimate = estimate,
      std_error = std_error,
      third = j
    )
  }
}

plot_data_pen <- bind_rows(plot_data_pen)

# Calculate the confidence intervals
plot_data_pen <- plot_data_pen %>%
  mutate(
    upper_ci = estimate + (1.96 * std_error),
    lower_ci = estimate - (1.96 * std_error)
  )

# Define colors for each penetration type

# Map reg_variable to the custom labels for the legend
plot_data_pen$kilde <- factor(plot_data_pen$reg_variable,
                              levels = c("solar_penetration_hat", "offshore_penetration_hat", "onshore_penetration_hat"),
                              labels = c("Sol pen", "Havvind pen", "Landvind pen"))

dep_variables_temp <- unique(plot_data_pen$dep_variable)
plots <- lapply(seq_along(dep_variables_temp), function(idx) {
  dep_var <- dep_variables_temp[idx]
  
  # Adjust the title based on the dep_variable
  title_text <- switch(dep_var,
                       "log_solar_ur" = "Sol afregningspris",
                       "log_offshore_ur" = "Havvind afregningspris",
                       "log_onshore_ur" = "Landvind afregningspris",
                       "solar_uf" = "Sol relativt til gns. pris",
                       "offshore_uf" = "Havvind relativt til gns. pris",
                       "onshore_uf" = "Landvind relativt til gns. pris",
                       dep_var)  # Default case if none of the above
  
  one_third <- split_points[idx+3, "one_third_x"]
  two_thirds <- split_points[idx+3, "two_thirds_x"]
  
  # Creating the plot for the current dep_var
  p <- ggplot(plot_data_pen %>% filter(dep_variable == dep_var), 
              aes(x = third, group = kilde, fill = kilde, color = kilde)) +
    geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), color = NA, alpha = 0.1) +
    geom_line(aes(y = estimate), linetype = "dashed") + 
    scale_fill_manual(values = kilde_farve) +
    scale_color_manual(values = kilde_farve) +
    scale_x_continuous(
      limits = c(1, 3),
      breaks = c(1, 2, 3),
      labels = c(paste("<", sprintf("%.1f", one_third), "%"), 
                 paste("[", sprintf("%.1f", one_third), "%;", sprintf("%.1f", two_thirds), "%]", sep = ""), 
                 paste(">", sprintf("%.1f", two_thirds), "%"))
    ) +
    scale_y_continuous(limits = c(-12, 16)) +  
    labs(title = title_text) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 20),
          axis.text.y = element_text(size = 20))  # Common theme settings
  
  p <- p + labs(title = title_text, 
                x = if (idx == 1) "Sol penetration niveau" else if (idx == 2) "Havvind penetration niveau" else "Landvind penetration niveau", 
                y = if (idx == 1) "Koefficient: Elasticiteten af afregningspris (PP)" else "")
  
  # Setting the title text size
  p <- p + theme(plot.title = element_text(size = 20),
                 axis.title.x = element_text(size = 20),
                 axis.title.y = element_text(size = 20))
  
  
  p <- p + theme(legend.position = "bottom", 
                 legend.title = element_blank(),
                 legend.text = element_text(size = 20))  # Setting legend text size
  
  # # Conditionally apply additional theme settings
  # if (idx == 2) {
  #   p <- p + theme(legend.position = "bottom", 
  #                  legend.title = element_blank(),
  #                  legend.text = element_text(size = 20))  # Setting legend text size
  # } else {
  #   p <- p + theme(legend.position = "none")  # Hides the legend for plots where idx is not 2
  # }
  return(p)  # Return the plot for each dep_variable
})

# Combine all the plots into a single object using patchwork
penetration_plot_DK2_CR <- patchwork::wrap_plots(plots, ncol = 3)
print(penetration_plot_DK2_CR)
# Save the final plot to a file
ggsave(filename = "penetration_plot_DK2_CR.png", plot = penetration_plot_DK2_CR, width = 16, height = 12, dpi = 300)

##### Plot af kommulativt niveaau for penetration - før 2021 samlet ####
cum_data <- data.frame()
# Apply the function to each variable for each region
cum_data <- rbind(cum_data,
                  transform(cumulative_count(data_pre_2021$solar_penetration), variable = "solar_penetration_hat"),
                  transform(cumulative_count(data_pre_2021$offshore_penetration), variable = "offshore_penetration_hat"),
                  transform(cumulative_count(data_pre_2021$onshore_penetration), variable = "onshore_penetration_hat"))

split_points <- data.frame()

for (var in unique(cum_data$variable)){
  var_data <- cum_data[cum_data$variable == var,]
  total <- max(var_data$cumcount)
  one_third_y = total/3
  one_third_x <- var_data$value[which.min(abs(var_data$cumcount - one_third_y))]
  two_thirds_y = 2*total/3
  two_thirds_x <- var_data$value[which.min(abs(var_data$cumcount - two_thirds_y))]
  
  
  split_points <- rbind(split_points, data.frame(variable = var, one_third_x = one_third_x, one_third_y = one_third_y, two_thirds_x = two_thirds_x, two_thirds_y = two_thirds_y))
}

cum_data$variable <- factor(cum_data$variable, 
                                    levels = c("solar_penetration_hat", "offshore_penetration_hat", "onshore_penetration_hat"))


cum_samlet <- ggplot(cum_data, aes(x = value, y = cumcount, color = variable)) +
  geom_line(linewidth = 2) + 
  scale_color_manual(
    values = c("solar_penetration_hat" = "orange", "offshore_penetration_hat" = "blue", "onshore_penetration_hat" = "brown"),
    labels = c("Sol pen", "Havvind", "Landvind")
  ) +
  geom_point(data = split_points, aes(x = one_third_x, y = one_third_y), color = "black", size = 3) +
  geom_point(data = split_points, aes(x = two_thirds_x, y = two_thirds_y), color = "black", size = 3) +
  
  labs(
    title = "",
    x = "Penetrationsrate (%)",
    y = "Kummulativt antal"
  ) +
  theme_minimal() +
  theme(
    legend.position = c(0.8, 0.1),
    legend.box = "horizontal",
    legend.text = element_text(size = 16),       # Set legend text size
    axis.title = element_text(size = 16),         # Set axis title size
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    plot.background = element_rect(fill = "white", colour = "white")
  ) +
  guides(
    color = guide_legend(title = NULL),
  ) +
  geom_blank(aes(color = variable))
ggsave("kummulativ_penetration_samlet.png", plot = cum_samlet, width = 10, height = 8, dpi = 300)
##### Regression opdelt på niveau af pen - før 2021 samlet #####

# Loop through each variable and run the regression
result_penetration <- list()  # Initialize list to store regression models
# har ændret data her
for (var_pen in c("solar_penetration_hat", "offshore_penetration_hat", "onshore_penetration_hat")) {
  
  one_third <- split_points$one_third_x[split_points$variable == var_pen]
  two_thirds <- split_points$two_thirds_x[split_points$variable == var_pen]
  
  # Subset the data into three parts based on split points
  for (i in 1:3) {
    if (i == 1) {
      data_subset <- data_pre_2021[data_pre_2021[[var_pen]] < one_third,]
    } else if (i == 2) {
      data_subset <- data_pre_2021[data_pre_2021[[var_pen]] >= one_third & data_pre_2021[[var_pen]] <= two_thirds,]
    } else {
      data_subset <- data_pre_2021[data_pre_2021[[var_pen]] > two_thirds,]
    }
    
    for (target in target_vars){
      # Run the regression for the current subset
      regression_model <- time_series_regression(target, predictors_pre_2021, data_subset)
      result_penetration[[var_pen]][[paste(i, "third")]][[target]] <- regression_model}
  }
}

##### Plot pen niveau - samlet - ur #####
#samling af data til plot
plot_data_pen <- list()

for (i in seq_along(target_vars)[1:3]) { #target var [1:3] angiver ur
  var <- target_vars[i]
  for (j in 1:3) { #penetrations niveau
    # Extract the ith estimate and standard error for the variable
    estimate <- result_penetration[[i]][[j]][[i]][["coeftest"]][, 1][i+1] #var_index+1
    std_error <- result_penetration[[i]][[j]][[i]][["coeftest"]][, 2][i+1]
    var_name = names(result_penetration[[i]][[j]][[i]][["coeftest"]][, 1])[i+1]
      
    # Create a tibble for this specific estimate and add it to the list
    plot_data_pen[[length(plot_data_pen) + 1]] <- tibble(
      dep_variable = var,
      reg_variable = var_name,
      estimate = estimate,
      std_error = std_error,
      third = j
    )
  }
}

plot_data_pen <- bind_rows(plot_data_pen)

# Calculate the confidence intervals
plot_data_pen <- plot_data_pen %>%
  mutate(
    upper_ci = estimate + (1.96 * std_error),
    lower_ci = estimate - (1.96 * std_error)
  )

# Define colors for each penetration type

# Map reg_variable to the custom labels for the legend
plot_data_pen$kilde <- factor(plot_data_pen$reg_variable,
                              levels = c("solar_penetration_hat", "offshore_penetration_hat", "onshore_penetration_hat"),
                              labels = c("Sol pen", "Havvind pen", "Landvind pen"))

dep_variables_temp <- unique(plot_data_pen$dep_variable)
plots <- lapply(seq_along(dep_variables_temp), function(idx) {
  dep_var <- dep_variables_temp[idx]
  
  # Adjust the title based on the dep_variable
  title_text <- switch(dep_var,
                       "log_solar_ur" = "Sol afregningspris",
                       "log_offshore_ur" = "Havvind afregningspris",
                       "log_onshore_ur" = "Landvind afregningspris",
                       "solar_uf" = "Sol relativt til gns. pris",
                       "offshore_uf" = "Havvind relativt til gns. pris",
                       "onshore_uf" = "Landvind relativt til gns. pris",
                       dep_var)  # Default case if none of the above
  
  one_third <- split_points[idx, "one_third_x"]
  two_thirds <- split_points[idx, "two_thirds_x"]
  
  # Creating the plot for the current dep_var
  p <- ggplot(plot_data_pen %>% filter(dep_variable == dep_var), 
              aes(x = third, group = kilde, fill = kilde, color = kilde)) +
    geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), color = NA, alpha = 0.1) +
    geom_line(aes(y = estimate), linetype = "dashed") + 
    scale_fill_manual(values = kilde_farve) +
    scale_color_manual(values = kilde_farve) +
    scale_x_continuous(
      limits = c(1, 3),
      breaks = c(1, 2, 3),
      labels = c(paste("<", sprintf("%.1f", one_third), "%"), 
                 paste("[", sprintf("%.1f", one_third), "%;", sprintf("%.1f", two_thirds), "%]", sep = ""), 
                 paste(">", sprintf("%.1f", two_thirds), "%"))
    ) +
    scale_y_continuous(limits = c(-23, 3)) + 
    theme_minimal() +
    theme(axis.text.x = element_text(size = 20),
          axis.text.y = element_text(size = 20))  # Common theme settings
  
  p <- p + labs(title = title_text, 
                x = if (idx == 1) "Sol penetration niveau" else if (idx == 2) "Havvind penetration niveau" else "Landvind penetration niveau", 
                y = if (idx == 1) "Koefficient: ln(Afregningspris) (EUR/MWh)" else "")
  
  # Setting the title text size
  p <- p + theme(plot.title = element_text(size = 20),
                 axis.title.x = element_text(size = 20),
                 axis.title.y = element_text(size = 20))
  
  p <- p + theme(legend.position = "bottom", 
                 legend.title = element_blank(),
                 legend.text = element_text(size = 20))  # Setting legend text size
  
  # if (idx == 2) {
  #   p <- p + theme(legend.position = "bottom", 
  #                  legend.title = element_blank(),
  #                  legend.text = element_text(size = 20))  # Setting legend text size
  # } else {
  #   p <- p + theme(legend.position = "none")  # Hides the legend for plots where idx is not 2
  # }
  
  return(p)  # Return the plot for each dep_variable
})

# Combine all the plots into a single object using patchwork
pen_plot <- patchwork::wrap_plots(plots, ncol = 3)
print(pen_plot)
# Save the final plot to a file
ggsave(filename = "penetration_plot_samlet_ur.png", plot = pen_plot, width = 16, height = 12, dpi = 300)

##### Plot pen niveau - samlet - CR #####
#samling af data til plot
plot_data_pen <- list()

for (i in seq_along(target_vars)[4:6]) { #target var [1:3] angiver ur
  var <- target_vars[i]
  for (j in 1:3) { #penetrations niveau
    # Extract the ith estimate and standard error for the variable
    estimate <- result_penetration[[i-3]][[j]][[i]][["coeftest"]][, 1][i-2] #var_index+1
    std_error <- result_penetration[[i-3]][[j]][[i]][["coeftest"]][, 2][i-2]
    var_name = names(result_penetration[[i-3]][[j]][[i]][["coeftest"]][, 1])[i-2]
    
    # Create a tibble for this specific estimate and add it to the list
    plot_data_pen[[length(plot_data_pen) + 1]] <- tibble(
      dep_variable = var,
      reg_variable = var_name,
      estimate = estimate,
      std_error = std_error,
      third = j
    )
  }
}

plot_data_pen <- bind_rows(plot_data_pen)

# Calculate the confidence intervals
plot_data_pen <- plot_data_pen %>%
  mutate(
    upper_ci = estimate + (1.96 * std_error),
    lower_ci = estimate - (1.96 * std_error)
  )

# Define colors for each penetration type

# Map reg_variable to the custom labels for the legend
plot_data_pen$kilde <- factor(plot_data_pen$reg_variable,
                              levels = c("solar_penetration_hat", "offshore_penetration_hat", "onshore_penetration_hat"),
                              labels = c("Sol pen", "Havvind pen", "Landvind pen"))

dep_variables_temp <- unique(plot_data_pen$dep_variable)
plots <- lapply(seq_along(dep_variables_temp), function(idx) {
  dep_var <- dep_variables_temp[idx]
  
  # Adjust the title based on the dep_variable
  title_text <- switch(dep_var,
                       "log_solar_ur" = "Sol afregningspris",
                       "log_offshore_ur" = "Havvind afregningspris",
                       "log_onshore_ur" = "Landvind afregningspris",
                       "solar_uf" = "Sol relativt til gns. pris",
                       "offshore_uf" = "Havvind relativt til gns. pris",
                       "onshore_uf" = "Landvind relativt til gns. pris",
                       dep_var)  # Default case if none of the above
  
  one_third <- split_points[idx, "one_third_x"]
  two_thirds <- split_points[idx, "two_thirds_x"]
  
  # Creating the plot for the current dep_var
  p <- ggplot(plot_data_pen %>% filter(dep_variable == dep_var), 
              aes(x = third, group = kilde, fill = kilde, color = kilde)) +
    geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), color = NA, alpha = 0.1) +
    geom_line(aes(y = estimate), linetype = "dashed") + 
    scale_fill_manual(values = kilde_farve) +
    scale_color_manual(values = kilde_farve) +
    scale_x_continuous(
      limits = c(1, 3),
      breaks = c(1, 2, 3),
      labels = c(paste("<", sprintf("%.1f", one_third), "%"), 
                 paste("[", sprintf("%.1f", one_third), "%;", sprintf("%.1f", two_thirds), "%]", sep = ""), 
                 paste(">", sprintf("%.1f", two_thirds), "%"))
    ) +
    scale_y_continuous(limits = c(-8, 10)) + 
    labs(title = title_text) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 20),
          axis.text.y = element_text(size = 20))  # Common theme settings
  
  p <- p + labs(title = title_text, 
                x = if (idx == 1) "Sol penetration niveau" else if (idx == 2) "Havvind penetration niveau" else "Landvind penetration niveau", 
                y = if (idx == 1) "Koefficient: %-vis elasticitet af afregningspris (PP)" else "")
  
  # Setting the title text size
  p <- p + theme(plot.title = element_text(size = 20),
                 axis.title.x = element_text(size = 20),
                 axis.title.y = element_text(size = 20))
  
  p <- p + theme(legend.position = "bottom", 
                 legend.title = element_blank(),
                 legend.text = element_text(size = 20))  # Setting legend text size
  
  # # Conditionally apply additional theme settings
  # if (idx == 2) {
  #   p <- p + theme(legend.position = "bottom", 
  #                  legend.title = element_blank(),
  #                  legend.text = element_text(size = 20))  # Setting legend text size
  # } else {
  #   p <- p + theme(legend.position = "none")  # Hides the legend for plots where idx is not 2
  # }
  return(p)  # Return the plot for each dep_variable
})

# Combine all the plots into a single object using patchwork
pen_plot_CR <- patchwork::wrap_plots(plots, ncol = 3)
print(pen_plot_CR)
# Save the final plot to a file
ggsave(filename = "penetration_plot_samlet_CR.png", plot = pen_plot_CR, width = 16, height = 12, dpi = 300)

##### Plot af  niveaau for penetration - før 2021 samlet ####
cum_data <- data.frame()
# Apply the function to each variable for each region
cum_data <- rbind(cum_data,
                  transform(cumulative_count(data_pre_2021$solar_penetration), variable = "solar_penetration_hat"),
                  transform(cumulative_count(data_pre_2021$offshore_penetration), variable = "offshore_penetration_hat"),
                  transform(cumulative_count(data_pre_2021$onshore_penetration), variable = "onshore_penetration_hat"))

split_points <- data.frame()

for (var in unique(cum_data$variable)){
  var_data <- cum_data[cum_data$variable == var,]
  total <- max(var_data$cumcount)
  one_third_y = total/3
  one_third_x <- var_data$value[which.min(abs(var_data$cumcount - one_third_y))]
  two_thirds_y = 2*total/3
  two_thirds_x <- var_data$value[which.min(abs(var_data$cumcount - two_thirds_y))]
  
  
  split_points <- rbind(split_points, data.frame(variable = var, one_third_x = one_third_x, one_third_y = one_third_y, two_thirds_x = two_thirds_x, two_thirds_y = two_thirds_y))
}

cum_data$variable <- factor(cum_data$variable, 
                            levels = c("solar_penetration_hat", "offshore_penetration_hat", "onshore_penetration_hat"))


cum_samlet <- ggplot(cum_data, aes(x = value, y = cumcount, color = variable)) +
  geom_line(linewidth = 2) + 
  scale_color_manual(
    values = c("solar_penetration_hat" = "orange", "offshore_penetration_hat" = "blue", "onshore_penetration_hat" = "brown"),
    labels = c("Sol pen", "Havvind", "Landvind")
  ) +
  geom_point(data = split_points, aes(x = one_third_x, y = one_third_y), color = "black", size = 3) +
  geom_point(data = split_points, aes(x = two_thirds_x, y = two_thirds_y), color = "black", size = 3) +
  
  labs(
    title = "",
    x = "Penetrationsrate (%)",
    y = "Kummulativt antal"
  ) +
  theme_minimal() +
  theme(
    legend.position = c(0.8, 0.1),
    legend.box = "horizontal",
    legend.text = element_text(size = 16),       # Set legend text size
    axis.title = element_text(size = 16),         # Set axis title size
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    plot.background = element_rect(fill = "white", colour = "white")
  ) +
  guides(
    color = guide_legend(title = NULL),
  ) +
  geom_blank(aes(color = variable))
ggsave("kummulativ_penetration_samlet.png", plot = cum_samlet, width = 10, height = 8, dpi = 300)
##### Regression opdelt på niveau af pen - før 2021 samlet - ikke log transformeret - vær opmærksom på foregående estimations estimater bliver slettet#####

# Loop through each variable and run the regression
result_penetration <- list()  # Initialize list to store regression models
# har ændret data her
for (var_pen in c("solar_penetration_hat", "offshore_penetration_hat", "onshore_penetration_hat")) {
  
  one_third <- split_points$one_third_x[split_points$variable == var_pen]
  two_thirds <- split_points$two_thirds_x[split_points$variable == var_pen]
  
  # Subset the data into three parts based on split points
  for (i in 1:3) {
    if (i == 1) {
      data_subset <- data_pre_2021[data_pre_2021[[var_pen]] < one_third,]
    } else if (i == 2) {
      data_subset <- data_pre_2021[data_pre_2021[[var_pen]] >= one_third & data_pre_2021[[var_pen]] <= two_thirds,]
    } else {
      data_subset <- data_pre_2021[data_pre_2021[[var_pen]] > two_thirds,]
    }
    
    for (target in target_vars_temp){
      # Run the regression for the current subset
      regression_model <- time_series_regression(target, predictors_pre_2021, data_subset)
      result_penetration[[var_pen]][[paste(i, "third")]][[target]] <- regression_model}
  }
}

##### Plot pen niveau - samlet - ur - ikke log transformeret #####
#samling af data til plot
plot_data_pen <- list()

for (i in seq_along(target_vars)[1:3]) { #target var [1:3] angiver ur
  var <- target_vars[i]
  for (j in 1:3) { #penetrations niveau
    # Extract the ith estimate and standard error for the variable
    estimate <- result_penetration[[i]][[j]][[i]][["coeftest"]][, 1][i+1] #var_index+1
    std_error <- result_penetration[[i]][[j]][[i]][["coeftest"]][, 2][i+1]
    var_name = names(result_penetration[[i]][[j]][[i]][["coeftest"]][, 1])[i+1]
    
    # Create a tibble for this specific estimate and add it to the list
    plot_data_pen[[length(plot_data_pen) + 1]] <- tibble(
      dep_variable = var,
      reg_variable = var_name,
      estimate = estimate,
      std_error = std_error,
      third = j
    )
  }
}

plot_data_pen <- bind_rows(plot_data_pen)

# Calculate the confidence intervals
plot_data_pen <- plot_data_pen %>%
  mutate(
    upper_ci = estimate + (1.96 * std_error),
    lower_ci = estimate - (1.96 * std_error)
  )

# Define colors for each penetration type

# Map reg_variable to the custom labels for the legend
plot_data_pen$kilde <- factor(plot_data_pen$reg_variable,
                              levels = c("solar_penetration_hat", "offshore_penetration_hat", "onshore_penetration_hat"),
                              labels = c("Sol pen", "Havvind pen", "Landvind pen"))

dep_variables_temp <- unique(plot_data_pen$dep_variable)
plots <- lapply(seq_along(dep_variables_temp), function(idx) {
  dep_var <- dep_variables_temp[idx]
  
  # Adjust the title based on the dep_variable
  title_text <- switch(dep_var,
                       "log_solar_ur" = "Sol afregningspris",
                       "log_offshore_ur" = "Havvind afregningspris",
                       "log_onshore_ur" = "Landvind afregningspris",
                       "solar_uf" = "Sol relativt til gns. pris",
                       "offshore_uf" = "Havvind relativt til gns. pris",
                       "onshore_uf" = "Landvind relativt til gns. pris",
                       dep_var)  # Default case if none of the above
  
  one_third <- split_points[idx, "one_third_x"]
  two_thirds <- split_points[idx, "two_thirds_x"]
  
  # Creating the plot for the current dep_var
  p <- ggplot(plot_data_pen %>% filter(dep_variable == dep_var), 
              aes(x = third, group = kilde, fill = kilde, color = kilde)) +
    geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), color = NA, alpha = 0.1) +
    geom_line(aes(y = estimate), linetype = "dashed") + 
    scale_fill_manual(values = kilde_farve) +
    scale_color_manual(values = kilde_farve) +
    scale_x_continuous(
      limits = c(1, 3),
      breaks = c(1, 2, 3),
      labels = c(paste("<", sprintf("%.1f", one_third), "%"), 
                 paste("[", sprintf("%.1f", one_third), "%;", sprintf("%.1f", two_thirds), "%]", sep = ""), 
                 paste(">", sprintf("%.1f", two_thirds), "%"))
    ) +
    scale_y_continuous(limits = c(-8, 1)) + 
    theme_minimal() +
    theme(axis.text.x = element_text(size = 20),
          axis.text.y = element_text(size = 20))  # Common theme settings
  
  p <- p + labs(title = title_text, 
                x = if (idx == 1) "Sol penetration niveau" else if (idx == 2) "Havvind penetration niveau" else "Landvind penetration niveau", 
                y = if (idx == 1) "Koefficient: ln(Afregningspris) (EUR/MWh)" else "")
  
  # Setting the title text size
  p <- p + theme(plot.title = element_text(size = 20),
                 axis.title.x = element_text(size = 20),
                 axis.title.y = element_text(size = 20))
  
  p <- p + theme(legend.position = "bottom", 
                 legend.title = element_blank(),
                 legend.text = element_text(size = 20))  # Setting legend text size
  
  # if (idx == 2) {
  #   p <- p + theme(legend.position = "bottom", 
  #                  legend.title = element_blank(),
  #                  legend.text = element_text(size = 20))  # Setting legend text size
  # } else {
  #   p <- p + theme(legend.position = "none")  # Hides the legend for plots where idx is not 2
  # }
  
  return(p)  # Return the plot for each dep_variable
})

# Combine all the plots into a single object using patchwork
pen_plot <- patchwork::wrap_plots(plots, ncol = 3)
print(pen_plot)
# Save the final plot to a file
ggsave(filename = "penetration_plot_samlet_ur_ikke_log.png", plot = pen_plot, width = 16, height = 12, dpi = 300)

##### Robustness check: inclusion of endogounes variabels - IV med endo #####
# long version

for (var in first_stage_target_vars){
  results[["First stage endo"]][[var]] = time_series_regression(var, first_stage_vars_non_season_endo, data_pre_2021_endo)
}

## Print first stage from 2021
stargazer(results[["First stage endo"]][[1]][[1]], results[["First stage endo"]][[2]][[1]], results[["First stage endo"]][[3]][[1]],
          title = "First stage regression",
          table.placement = "H",
          label="tab:first_stage",
          header = FALSE,
          type = "latex",
          column.labels = c("Sol", "Havvind", "Landvind"),
          column.sep.width = "10pt",
          align = TRUE,  # Assuming you want centered alignment. Adjust as needed.
          dep.var.labels.include = TRUE,
          model.names = TRUE,
          digits=2,
          out = "firststage_regression.tex",
          covariate.labels = c("sol pen", "Havvind pen", "Landvind pen", "Gas pen", "Netto export pen", "Forbrug", "Temperatur", "Gas Pris"),
          keep = first_stage_vars_non_season_endo[1:8], 
          add.lines = list(
            c("Antal observationer", format(results[["First stage endo"]][[1]][[2]], digits = 2, nsmall = 2), format(results[["First stage endo"]][[2]][[2]], digits = 2, nsmall = 2), format(results[["First stage endo"]][[3]][[2]], digits = 2, nsmall = 2)),
            c("Frihedsgrader", format(as.numeric(attr(results[["First stage endo"]][[1]][["coeftest"]], "df")), digits = 2),
              format(as.numeric(attr(results[["First stage endo"]][[2]][["coeftest"]], "df")), digits = 2),
              format(as.numeric(attr(results[["First stage endo"]][[3]][["coeftest"]], "df")), digits = 2)),
            c("R-squared", format(results[["First stage endo"]][[1]][[3]], digits = 2, nsmall = 2), format(results[["First stage endo"]][[2]][[3]], digits = 2, nsmall = 2), format(results[["First stage endo"]][[3]][[3]], digits = 2, nsmall = 2)),
            c("Justeret R-squared", format(results[["First stage endo"]][[1]][[4]], digits = 2, nsmall = 2), format(results[["First stage endo"]][[2]][[4]], digits = 2, nsmall = 2), format(results[["First stage endo"]][[3]][[4]], digits = 2, nsmall = 2)),
            c("F Statistik", format(results[["First stage endo"]][[1]][[5]], digits = 2, nsmall = 2), format(results[["First stage endo"]][[2]][[5]], digits = 2, nsmall = 2), format(results[["First stage endo"]][[3]][[5]], digits = 2, nsmall = 2)),
            c("Standardfejl", "HAC", "HAC", "HAC"),
            c("Års dummies", "Ja", "Ja", "Ja"),
            c("Årlige og ugentlige fouriertransformationer", "Ja", "Ja", "Ja")
          )
)

data_pre_2021_endo$solar_penetration_hat = results[["First stage endo"]][[1]][["fitted_values"]]
data_pre_2021_endo$offshore_penetration_hat = results[["First stage endo"]][[2]][["fitted_values"]]
data_pre_2021_endo$onshore_penetration_hat = results[["First stage endo"]][[3]][["fitted_values"]]

for (var in target_vars) {
  results[["second stage endo"]][[var]] <- time_series_regression(var, predictors_robust_endo, data_pre_2021_endo)
}


stargazer(results[["second stage endo"]][[1]][[1]],results[["second stage endo"]][[2]][[1]],results[["second stage endo"]][[3]][[1]], results[["second stage endo"]][[4]][[1]],results[["second stage endo"]][[5]][[1]],results[["second stage endo"]][[6]][[1]],
          title = "Robust: Regression med endogene variable",
          table.placement = "H",
          label="tab:endo_res_from_2021_ur",
          header = FALSE,
          type = "latex",
          column.labels = c("Sol", "Havvind", "Landvind", "Sol", "Havvind", "Landvind"),
          column.sep.width = "10pt",
          align = ,
          dep.var.labels.include = FALSE,
          model.names = FALSE,
          digits=2,
          covariate.labels = c("Sol pen", "Havvind pen", "Landvind pen", "Gas pen", "Netto export pen", "Forbrug", "Temperatur", "Gas Pris"),
          keep = vars_non_season_endo[1:8], 
          add.lines = list(
            c("Antal observationer", format(results[["second stage endo"]][[1]][[2]], digits = 2, nsmall = 2), format(results[["second stage endo"]][[2]][[2]], digits = 2, nsmall = 2), format(results[["second stage endo"]][[3]][[2]], digits = 2, nsmall = 2), format(results[["second stage endo"]][[4]][[2]], digits = 2, nsmall = 2), format(results[["second stage endo"]][[5]][[2]], digits = 2, nsmall = 2), format(results[["second stage endo"]][[6]][[2]]), digits = 2, nsmall = 2),
            c("R-squared", format(results[["second stage endo"]][[1]][[3]], digits = 2, nsmall = 2), format(results[["second stage endo"]][[2]][[3]], digits = 2, nsmall = 2), format(results[["second stage endo"]][[3]][[3]], digits = 2, nsmall = 2), format(results[["second stage endo"]][[4]][[3]], digits = 2, nsmall = 2), format(results[["second stage endo"]][[5]][[3]], digits = 2, nsmall = 2), format(results[["second stage endo"]][[6]][[3]], digits = 2, nsmall = 2)),
            c("Justeret R-squared", format(results[["second stage endo"]][[1]][[4]], digits = 2, nsmall = 2), format(results[["second stage endo"]][[2]][[4]], digits = 2, nsmall = 2), format(results[["second stage endo"]][[3]][[4]], digits = 2, nsmall = 2), format(results[["second stage endo"]][[4]][[4]], digits = 2, nsmall = 2), format(results[["second stage endo"]][[5]][[4]], digits = 2, nsmall = 2), format(results[["second stage endo"]][[6]][[4]], digits = 2, nsmall = 2)),
            c("Standardfejl", "HAC", "HAC", "HAC", "HAC", "HAC", "HAC"),
            c("Fourier transformationer", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja"),
            c("Weekend og års dummies", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja")))

##### Robustness check: inclusion of endogounes variabels - IV uden endo####

for (var in target_vars) {
  results[["second stage endo - IV uden endo"]][[var]] <- time_series_regression(var, predictors_robust_endo, data_pre_2021_endo)
}


stargazer(results[["second stage endo - IV uden endo"]][[1]][[1]],results[["second stage endo - IV uden endo"]][[2]][[1]],results[["second stage endo - IV uden endo"]][[3]][[1]], results[["second stage endo - IV uden endo"]][[4]][[1]],results[["second stage endo - IV uden endo"]][[5]][[1]],results[["second stage endo - IV uden endo"]][[6]][[1]],
          title = "Robust: Regression med endogene variable",
          table.placement = "H",
          label="tab:endo_res_from_2021_ur",
          header = FALSE,
          type = "latex",
          column.labels = c("Sol", "Havvind", "Landvind", "Sol", "Havvind", "Landvind"),
          column.sep.width = "10pt",
          align = ,
          dep.var.labels.include = FALSE,
          model.names = FALSE,
          digits=2,
          covariate.labels = c("Sol pen", "Havvind pen", "Landvind pen", "Gas pen", "Netto export pen", "Forbrug", "Temperatur", "Gas Pris"),
          keep = vars_non_season_endo[1:8], 
          add.lines = list(
            c("Antal observationer", format(results[["second stage endo - IV uden endo"]][[1]][[2]], digits = 2, nsmall = 2), format(results[["second stage endo - IV uden endo"]][[2]][[2]], digits = 2, nsmall = 2), format(results[["second stage endo - IV uden endo"]][[3]][[2]], digits = 2, nsmall = 2), format(results[["second stage endo - IV uden endo"]][[4]][[2]], digits = 2, nsmall = 2), format(results[["second stage endo - IV uden endo"]][[5]][[2]], digits = 2, nsmall = 2), format(results[["second stage endo - IV uden endo"]][[6]][[2]]), digits = 2, nsmall = 2),
            c("R-squared", format(results[["second stage endo - IV uden endo"]][[1]][[3]], digits = 2, nsmall = 2), format(results[["second stage endo - IV uden endo"]][[2]][[3]], digits = 2, nsmall = 2), format(results[["second stage endo - IV uden endo"]][[3]][[3]], digits = 2, nsmall = 2), format(results[["second stage endo - IV uden endo"]][[4]][[3]], digits = 2, nsmall = 2), format(results[["second stage endo - IV uden endo"]][[5]][[3]], digits = 2, nsmall = 2), format(results[["second stage endo - IV uden endo"]][[6]][[3]], digits = 2, nsmall = 2)),
            c("Justeret R-squared", format(results[["second stage endo - IV uden endo"]][[1]][[4]], digits = 2, nsmall = 2), format(results[["second stage endo - IV uden endo"]][[2]][[4]], digits = 2, nsmall = 2), format(results[["second stage endo - IV uden endo"]][[3]][[4]], digits = 2, nsmall = 2), format(results[["second stage endo - IV uden endo"]][[4]][[4]], digits = 2, nsmall = 2), format(results[["second stage endo - IV uden endo"]][[5]][[4]], digits = 2, nsmall = 2), format(results[["second stage endo - IV uden endo"]][[6]][[4]], digits = 2, nsmall = 2)),
            c("Standardfejl", "HAC", "HAC", "HAC", "HAC", "HAC", "HAC"),
            c("Fourier transformationer", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja"),
            c("Weekend og års dummies", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja")))

##### Regression of output for forbrug ######
# first_stage_forbrug_var <- c("is_weekend", "solar_penetration", "offshore_penetration", "onshore_penetration", "temperature", "D_2016","D_2017","D_2018", "D_2019","D_2020", "fourier_h_7_cos", "fourier_h_7_sin","fourier_h_365_cos", "fourier_h_365_sin", "region_dummy",)
# second_stage_forbrug_var <- c("is_weekend", "solar_penetration", "offshore_penetration", "onshore_penetration", "temperature", "D_2016","D_2017","D_2018","D_2019","D_2020", "fourier_h_7_cos", "fourier_h_7_sin", "fourier_h_365_sin", "region_dummy", "load_hat")
# first_stage_results <- panel_fe_regression("Load", first_stage_forbrug_var, data_pre_2021)
# data_pre_2021$load_hat = first_stage_results[[6]]
# 
# second_stage_forbrug_res <- list()  # Initialize list to store regression models
# for (var in target_vars) {
#   second_stage_forbrug_res[[var]] <- panel_fe_regression(var, second_stage_forbrug_var, data_pre_2021)
# }
# 
# #first stage
# stargazer(first_stage_results[[1]], second_stage_forbrug_res[[1]][[1]],second_stage_forbrug_res[[2]][[1]],second_stage_forbrug_res[[3]][[1]], second_stage_forbrug_res[[4]][[1]],second_stage_forbrug_res[[5]][[1]],second_stage_forbrug_res[[6]][[1]],
#           title = "First og second stage regression med kontrol for forbrug",
#           table.placement = "H",
#           label="tab:firststage_res",
#           header = FALSE,
#           type = "latex",
#           column.labels = "Estiamted load",
#           column.sep.width = "10pt",
#           align = ,
#           dep.var.labels.include = FALSE,
#           model.names = FALSE,
#           digits=2,
#           out = "firststage_regression.tex",
#           #covariate.labels = predictors_second_stage <- c("Sol pen", "Havvind pen", "Landvind pen", "$Fourier(cos)_{days=7}$", "$Fourier(sin)_{days=7}$", "$Fourier(sin)_{days=365}$", "$Fourier(cos)_{days=365}$", "load_hat"),
#           omit = c("region_dummy", "is_weekend", "temperature", "D_2016","D_2017","D_2018","D_2016","D_2019","D_2020","D_2021","D_2022","D_2023", "region_dummy", "constant"),
#           add.lines = list(
#             c("Antal observationer", format(first_stage_results[[2]], digits = 2, nsmall = 2), format(second_stage_forbrug_res[[1]][[2]], digits = 2, nsmall = 2), format(second_stage_forbrug_res[[2]][[2]], digits = 2, nsmall = 2), format(second_stage_forbrug_res[[3]][[2]], digits = 2, nsmall = 2), format(second_stage_forbrug_res[[4]][[2]], digits = 2, nsmall = 2), format(second_stage_forbrug_res[[5]][[2]], digits = 2, nsmall = 2), format(second_stage_forbrug_res[[6]][[2]], digits = 2, nsmall = 2)),
#             c("R-squared", format(first_stage_results[[3]], digits = 2, nsmall = 2), format(second_stage_forbrug_res[[1]][[3]], digits = 2, nsmall = 2), format(second_stage_forbrug_res[[2]][[3]], digits = 2, nsmall = 2), format(second_stage_forbrug_res[[3]][[3]], digits = 2, nsmall = 2), format(second_stage_forbrug_res[[4]][[3]], digits = 2, nsmall = 2), format(second_stage_forbrug_res[[5]][[3]], digits = 2, nsmall = 2), format(second_stage_forbrug_res[[6]][[3]], digits = 2, nsmall = 2)),
#             c("Justeret R-squared", format(first_stage_results[[4]], digits = 2, nsmall = 2), format(second_stage_forbrug_res[[1]][[4]], digits = 2, nsmall = 2), format(second_stage_forbrug_res[[2]][[4]], digits = 2, nsmall = 2), format(second_stage_forbrug_res[[3]][[4]], digits = 2, nsmall = 2), format(second_stage_forbrug_res[[4]][[4]], digits = 2, nsmall = 2), format(second_stage_forbrug_res[[5]][[4]], digits = 2, nsmall = 2), format(second_stage_forbrug_res[[6]][[4]], digits = 2, nsmall = 2)),
#             c("F Statistik", format(first_stage_results[[5]], digits = 2, nsmall = 2), format(second_stage_forbrug_res[[1]][[5]], digits = 2, nsmall = 2), format(second_stage_forbrug_res[[2]][[5]], digits = 2, nsmall = 2), format(second_stage_forbrug_res[[3]][[5]], digits = 2, nsmall = 2), format(second_stage_forbrug_res[[4]][[5]], digits = 2, nsmall = 2), format(second_stage_forbrug_res[[5]][[5]], digits = 2, nsmall = 2), format(second_stage_forbrug_res[[6]][[5]], digits = 2, nsmall = 2)),
#             c("Standardfejl", "HAC", "HAC", "HAC", "HAC", "HAC", "HAC", "HAC"),
#             c("Fourier transformationer", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja"),
#             c("Weekend og års dummies", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja")))

first_stage_forbrug_var <- c("is_weekend", "solar_penetration", "offshore_penetration", "onshore_penetration", "temperature", "D_2016","D_2017","D_2018", "D_2019","D_2020", "D_2021","D_2022","D_2023", "fourier_h_7_cos", "fourier_h_7_sin","fourier_h_365_cos", "fourier_h_365_sin", "region_dummy")
second_stage_forbrug_var <- c("is_weekend", "solar_penetration", "offshore_penetration", "onshore_penetration", "temperature", "D_2016","D_2017","D_2018","D_2019","D_2020", "D_2021","D_2022","D_2023", "fourier_h_7_cos", "fourier_h_7_sin", "fourier_h_365_sin", "region_dummy", "load_hat")
robust_check_data = data
first_stage_results <- panel_fe_regression("load", first_stage_forbrug_var, robust_check_data)
robust_check_data$load_hat <- first_stage_results[[6]]

second_stage_forbrug_res <- list()  # Initialize list to store regression models
for (var in target_vars) {
  second_stage_forbrug_res[[var]] <- panel_fe_regression(var, second_stage_forbrug_var, robust_check_data)
}

#first stage
stargazer(first_stage_results[[1]], second_stage_forbrug_res[[1]][[1]],second_stage_forbrug_res[[2]][[1]],second_stage_forbrug_res[[3]][[1]], second_stage_forbrug_res[[4]][[1]],second_stage_forbrug_res[[5]][[1]],second_stage_forbrug_res[[6]][[1]],
          title = "First og second stage regression med kontrol for forbrug",
          table.placement = "H",
          label="tab:firststage_res",
          header = FALSE,
          type = "latex",
          column.labels = "Estiamted load",
          column.sep.width = "10pt",
          align = ,
          dep.var.labels.include = FALSE,
          model.names = FALSE,
          digits=2,
          out = "firststage_regression.tex",
          covariate.labels <- c("Sol pen", "Havvind pen", "Landvind pen", "$Fourier(cos)_{days=7}$", "$Fourier(sin)_{days=7}$", "$Fourier(sin)_{days=365}$", "$Fourier(cos)_{days=365}$", "load_hat"),
          omit = c("region_dummy", "is_weekend", "temperature", "D_2016","D_2017","D_2018","D_2016","D_2019","D_2020","D_2021","D_2022","D_2023", "region_dummy", "Constant"),
          add.lines = list(
            c("Antal observationer", format(first_stage_results[[2]], digits = 2, nsmall = 2), format(second_stage_forbrug_res[[1]][[2]], digits = 2, nsmall = 2), format(second_stage_forbrug_res[[2]][[2]], digits = 2, nsmall = 2), format(second_stage_forbrug_res[[3]][[2]], digits = 2, nsmall = 2), format(second_stage_forbrug_res[[4]][[2]], digits = 2, nsmall = 2), format(second_stage_forbrug_res[[5]][[2]], digits = 2, nsmall = 2), format(second_stage_forbrug_res[[6]][[2]], digits = 2, nsmall = 2)),
            c("R-squared", format(first_stage_results[[3]], digits = 2, nsmall = 2), format(second_stage_forbrug_res[[1]][[3]], digits = 2, nsmall = 2), format(second_stage_forbrug_res[[2]][[3]], digits = 2, nsmall = 2), format(second_stage_forbrug_res[[3]][[3]], digits = 2, nsmall = 2), format(second_stage_forbrug_res[[4]][[3]], digits = 2, nsmall = 2), format(second_stage_forbrug_res[[5]][[3]], digits = 2, nsmall = 2), format(second_stage_forbrug_res[[6]][[3]], digits = 2, nsmall = 2)),
            c("Justeret R-squared", format(first_stage_results[[4]], digits = 2, nsmall = 2), format(second_stage_forbrug_res[[1]][[4]], digits = 2, nsmall = 2), format(second_stage_forbrug_res[[2]][[4]], digits = 2, nsmall = 2), format(second_stage_forbrug_res[[3]][[4]], digits = 2, nsmall = 2), format(second_stage_forbrug_res[[4]][[4]], digits = 2, nsmall = 2), format(second_stage_forbrug_res[[5]][[4]], digits = 2, nsmall = 2), format(second_stage_forbrug_res[[6]][[4]], digits = 2, nsmall = 2)),
            c("F Statistik", format(first_stage_results[[5]], digits = 2, nsmall = 2), format(second_stage_forbrug_res[[1]][[5]], digits = 2, nsmall = 2), format(second_stage_forbrug_res[[2]][[5]], digits = 2, nsmall = 2), format(second_stage_forbrug_res[[3]][[5]], digits = 2, nsmall = 2), format(second_stage_forbrug_res[[4]][[5]], digits = 2, nsmall = 2), format(second_stage_forbrug_res[[5]][[5]], digits = 2, nsmall = 2), format(second_stage_forbrug_res[[6]][[5]], digits = 2, nsmall = 2)),
            c("Standardfejl", "HAC", "HAC", "HAC", "HAC", "HAC", "HAC", "HAC"),
            c("Fourier transformationer", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja"),
            c("Weekend og års dummies", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja")))


##### Fourier periodogram #####
plot_periodogram <- function(y, fs, title, log_scale) {
  # Define the file name based on the title to avoid overwriting files
  filename <- paste0(gsub(" ", "_", title), ".png")
  
  # Start PNG device
  png(filename, width = 800, height = 600)
  
  # Check if 'y' is a dataframe and convert it to a vector if it is
  if (is.data.frame(y)) {
    y <- y[[1]]  # Assuming 'y' is a single column dataframe
  }
  
  # Replace NA values with 0
  y[is.na(y)] <- 0
  
  # Calculate the periodogram
  pgram <- spectrum(y, log = "no", plot = FALSE)
  
  # Convert frequency to period
  period <- 1 / pgram$freq[-1]
  
  log_period <- log(period) / log(log_scale)
  
  # Plot the periodogram with log scale on the x-axis
  plot(log_period, pgram$spec[-1], type = 'l',
       xlab = '',  # Will be added later
       ylab = 'Spektral densitet',
       #main = title,
       xaxt = 'n',
       cex.lab = 1.2,  # Increase label size
       cex.main = 1.2) # Suppress the x-axis
  
  # Invert the x-axis after plotting
  usr <- par("usr")
  par(usr = c(usr[1], usr[2], usr[3], usr[4]))
  
  # Define custom tick labels and values
  #tick_labels <- c("Halv ugentlige (3,5 dage)", "Ugentlige (7days)", "Månedlige (30 days)","Årlig (365 days)" )
  tick_values <- log(c(3.5, 7, 30, 365.25)) / log(log_scale)
  
  # Add the custom ticks
  axis(1, at = tick_values, labels = FALSE)
  
  # Manually add labels, shifting the "Weekly" label downward
  mtext(side = 1, line = 0.5, at = tick_values[1], "Halv ugentlige (3,5 dage)")
  mtext(side = 1, line = 2.5, at = tick_values[2], "Ugentlige (7dage)")  # Shifted downwards
  mtext(side = 1, line = 0.5, at = tick_values[3], "Månedlige (30 dage)")
  mtext(side = 1, line = 0.5, at = tick_values[4], "Årlig (365 dage)")
  
  # Set the x-label
  title(xlab = "Period (days)", line = 5, cex.lab = 0.75)
  
  # Close the PNG device
  dev.off()
}


regions <- c('DK_1', 'DK_2')
prices <- c('load', 'price', 'Solar_ur', 'Wind_Offshore_ur', 'Wind_Onshore_ur', 'Solar_uf', 'Wind_Offshore_uf', 'Wind_Onshore_uf') # Det er reelt kun load der er intressant

for (region in regions) {
  for (price in prices) {
    # Extract the time series data for the current region and price
    y <- data_pre_2021[data_pre_2021$region == region, price, drop = FALSE]
    
    # Check if 'y' is a dataframe and convert it to a vector if it is
    if (is.data.frame(y)) {
      y <- y[[1]]  # Assuming 'y' is a single column dataframe
    }
    
    # Replace NA values with 0
    y[is.na(y)] <- 0
    
    # print(paste("Length of y for", region, price, ":", length(y)))
    
    # Define the sampling frequency
    fs <- 1.0  # Sampling frequency (1/day for daily data)
    
    # Ensure that y is a numeric vector and not NULL
    if (is.numeric(y) && length(y) > 0) {
      # Call the plot_periodogram function
      plot_title <- sprintf("Periodogram for %s, %s", region, price)
      
      # Create the plot
      plot_periodogram(y, fs, plot_title, 7)
    } else {
      warning(sprintf("Data for %s, %s is not available or not numeric.", region, price))
    }
  }
}

##### Unit root test ADF og PP #####
#SIC method
# Make sure to install and load the urca package
# install.packages("urca")


unit_root_test_function <- function(df, var_name, max_k = 200) {
  # Remove NA values from the series
  print(var_name)
  df_clean <- na.omit(df[[var_name]])
  
  n <- length(df_clean)
  df_clean <- df_clean[1:n]
  
  # Compute the SIC for each lag length
  sic_values <- sapply(1:max_k, function(k_val) {
    lagged_diff <- lag(diff(df_clean), k_val)
    valid_indices <- (k_val + 2):n
    model <- lm(diff(df_clean)[valid_indices] ~ df_clean[valid_indices] + lagged_diff[valid_indices])
    log_lik <- logLik(model)
    k <- length(coefficients(model))
    sic <- -2 * as.numeric(log_lik) + k * log(length(valid_indices))
    return(sic)
  })
  
  # Select the lag that minimizes the SIC
  optimal_k <- which.min(sic_values)
  
  adf_result <- adf.test(df_clean, alternative = "stationary", k = optimal_k)
  
  # PP Test
  pp_result <- PP.test(df_clean)
  
  # Initialize return values
  adf_stat <- ifelse(!is.null(adf_result$statistic), adf_result$statistic, NA)
  adf_p <- ifelse(!is.null(adf_result$p.value), adf_result$p.value, NA)
  pp_stat <- ifelse(!is.null(pp_result$statistic), pp_result$statistic, NA)
  pp_p <- ifelse(!is.null(pp_result$p.value), pp_result$p.value, NA)
  
  return(list(
    pp_stat = pp_stat,
    pp_p = pp_p
  ))
}

vars <- c("p_solar_penetration", "p_offshore_penetration", "p_onshore_penetration", "solar_penetration_hat", "offshore_penetration_hat", "onshore_penetration_hat", "log_solar_ur", "log_offshore_ur", "log_onshore_ur", "solar_uf", "offshore_uf", "onshore_uf", "load", "net_export_penetration", "fossil_gas_penetration", "gas_price" )

stationaritets_test_pre_2021 <- list()

for (var in vars) {
  stationaritets_test_pre_2021[["DK1"]][[var]] <- unit_root_test_function(data_pre_2021_DK1, var)
  stationaritets_test_pre_2021[["DK2"]][[var]] <- unit_root_test_function(data_pre_2021_DK2, var)
}

vars <- c("p_solar_penetration", "p_offshore_penetration", "p_onshore_penetration", "solar_penetration_hat", "offshore_penetration_hat", "onshore_penetration_hat", "log_solar_ur", "log_offshore_ur", "log_onshore_ur", "solar_uf", "offshore_uf", "onshore_uf")

stationaritets_test_from_2021 <- list()

for (var in vars) {
  stationaritets_test_from_2021[["DK1"]][[var]] <- unit_root_test_function(data_from_2021_DK1, var)
  stationaritets_test_from_2021[["DK2"]][[var]] <- unit_root_test_function(data_from_2021_DK2, var)
}

##### residualplot #####
#Demand
data_temp = data[3:500,]
data_temp$solar_ur_hat = result[["solar_ur"]][["fitted_values"]]
data_temp$offshore_ur_hat = result[["offshore_ur"]][["fitted_values"]]
data_temp$onshore_ur_hat = result[["onshore_ur"]][["fitted_values"]]

ggplot(data_temp, aes(x = date, y = p_solar - solar_ur ))+ geom_point(alpha = 0.6) +labs(x = "Estimated Load (load_hat)", y = "Actual Load", title = "Scatter plot of Actual vs Estimated Load") +theme_minimal()
ggplot(data_temp, aes(x = date, y = p_offshore - solar_ur ))+ geom_point(alpha = 0.6) +labs(x = "Estimated Load (load_hat)", y = "Actual Load", title = "Scatter plot of Actual vs Estimated Load") +theme_minimal()
ggplot(data_temp, aes(x = date, y = p_onshore - solar_ur ))+ geom_point(alpha = 0.6) +labs(x = "Estimated Load (load_hat)", y = "Actual Load", title = "Scatter plot of Actual vs Estimated Load") +theme_minimal()


ggplot(data_temp, aes(x = date, y = solar_ur ))+ geom_point(alpha = 0.6) +labs(x = "Estimated Load (load_hat)", y = "Actual Load", title = "Scatter plot of Actual vs Estimated Load") +theme_minimal()

ggplot(data, aes(x = date, y = Load-load_hat)) + geom_point(alpha = 0.6) + labs(x = "Date", y = "Actual Load", title = "Development in load") + theme_minimal()

#ur
ggplot(data, aes(x = date, y = solar_ur)) + geom_point(alpha = 0.6) + labs(x = "Date", y = "Actual Load", title = "Development in load") + theme_minimal()
ggplot(data, aes(x = date, y = offshore_ur)) + geom_point(alpha = 0.6) + labs(x = "Date", y = "Actual Load", title = "Development in load") + theme_minimal()
ggplot(data, aes(x = date, y = onshore_ur)) + geom_point(alpha = 0.6) + labs(x = "Date", y = "Actual Load", title = "Development in load") + theme_minimal()

#CF
ggplot(data, aes(x = date, y = solar_uf)) + geom_point(alpha = 0.6) + labs(x = "Date", y = "Actual Load", title = "Development in load") + theme_minimal()
ggplot(data, aes(x = date, y = offshore_uf)) + geom_point(alpha = 0.6) + labs(x = "Date", y = "Actual Load", title = "Development in load") + theme_minimal()
ggplot(data, aes(x = date, y = onshore_uf)) + geom_point(alpha = 0.6) + labs(x = "Date", y = "Actual Load", title = "Development in load") + theme_minimal()




##### plot af enhedspris og relativ pris ####

# Reshape data to long format
long_data <- data %>%
  pivot_longer(cols = c(solar_ur, offshore_ur, onshore_ur, solar_uf, offshore_uf, onshore_uf),
               names_to = "variable", values_to = "value")

# Create labels for the plot
long_data$label <- case_when(
  long_data$variable == "solar_ur"    ~ "Sol afregningspris",
  long_data$variable == "offshore_ur" ~ "Havvind afregningspris",
  long_data$variable == "onshore_ur"  ~ "Landvind afregningspris",
  long_data$variable == "solar_uf"    ~ "Sol markedsværdifaktor",
  long_data$variable == "offshore_uf" ~ "Havvind markedsværdifaktor",
  long_data$variable == "onshore_uf"  ~ "Landvind markedsværdifaktor",
  TRUE                                ~ as.character(long_data$variable)
)

long_data$label <- factor(long_data$label,
                          levels = c("Sol afregningspris", "Havvind afregningspris", "Landvind afregningspris",
                                     "Sol markedsværdifaktor", "Havvind markedsværdifaktor", "Landvind markedsværdifaktor"))
# Group by month and variable, then calculate the mean
monthly_avg_data <- long_data %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month, label) %>%
  summarise(monthly_avg = mean(value, na.rm = TRUE))

monthly_avg_data$label <- factor(monthly_avg_data$label, 
                                 levels = c("Sol afregningspris", "Havvind afregningspris", "Landvind afregningspris", "Sol markedsværdifaktor", "Havvind markedsværdifaktor", "Landvind markedsværdifaktor"),
                                 labels = c("Sol afregningspris", "Havvind afregningspris", "Landvind afregningspris", "Sol markedsværdifaktor", "Havvind markedsværdifaktor", "Landvind markedsværdifaktor"))

# Filter ur monthly average data
ur_monthly_avg <- monthly_avg_data %>% filter(label %in% c("Sol afregningspris", "Havvind afregningspris", "Landvind afregningspris"))

# Correct typo in color mapping and match it with the labels
ur_color_map <- c("Sol afregningspris" = "orange", "Havvind afregningspris" = "blue", "Landvind afregningspris" = "brown")

# Create ur plot with monthly average
ur_plot_tid <- ggplot(ur_monthly_avg, aes(x = month, y = monthly_avg, color = label)) +
  geom_line() +
  geom_vline(xintercept = as.POSIXct("2021-01-01"), linetype="dotted", color="black", size =1.5) +
  scale_color_manual(values = ur_color_map, labels = c("Sol", "Havvind", "Landvind")) +
  labs(x = "", y = "Afregningspris (AP) (EUR/MWh)", color = NULL) +
  theme_minimal() +
  theme(
    legend.position = c(0.3, 0.8),
    legend.box = "horizontal",
    legend.text = element_text(size = 16),
    axis.title = element_text(size = 16),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    plot.background = element_rect(fill = "white", colour = "white")
  )

# Print ur monthly average plot
print(ur_plot_tid)


# Filter uf monthly average data
uf_monthly_avg <- monthly_avg_data %>% filter(grepl("relativ pris", label))

# Define colors for uf variables
uf_color_map <- c("Sol markedsværdifaktor" = "orange", "Havvind markedsværdifaktor" = "blue", "Landvind markedsværdifaktor" = "brown")

# Create uf plot with monthly average
CR_plot_tid <- ggplot(uf_monthly_avg, aes(x = month, y = monthly_avg, color = label, group = label)) +
  geom_line() +
  geom_vline(xintercept = as.POSIXct("2021-01-01"), linetype="dotted", color="black", size =1.5) +
  scale_color_manual(values = uf_color_map) +
  labs(x = "", y = "Markedsværdifaktoren (MVF) (%)") +
  theme_minimal() +
  theme(legend.position = "none",
        legend.box = "horizontal",
        legend.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        plot.background = element_rect(fill = "white", colour = "white")
  )

# Save the ur plot
ggsave("ur_plot_tid.png", ur_plot_tid, width = 10, height = 6, dpi = 300)

# Save the CR plot
ggsave("CR_plot_tid.png", CR_plot_tid, width = 10, height = 6, dpi = 300)



##### spline #####
library(mgcv)

# formula <- as.formula(
#   paste(response, "~", s("", bs="CR") + paste(predictors, collapse=" + "), data=data_pre_2021)
# )

#model_sol <- gam(y ~ s(x, bs="cr"), data=data)

formula <- as.formula(
  paste("log_solar_ur", "~", s(x, bs="CR")+ paste(predictors, collapse=" + "), data=data_pre_2021)
)

plot(model_sol)

model_hav <- gam(y ~ s(x, bs="cr"), data=data)
formula <- as.formula(
  paste("log_offshore_ur", "~", s(x, bs="CR")+ paste(predictors, collapse=" + "), data=data_pre_2021)
)
plot(model_sol)

model_land <- gam(y ~ s(x, bs="cr"), data=data)
formula <- as.formula(
  paste("log_onshore_ur", "~", s(x, bs="CR")+ paste(predictors, collapse=" + "), data=data_pre_2021)
)
plot(model_sol)
