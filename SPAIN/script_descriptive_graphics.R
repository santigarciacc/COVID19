# Remove all the variables
rm(list = ls())
assign("last.warning", NULL, envir = baseenv()) # Clean warnings

# DO NOT CHANGE: libraries required: Rcode that we will need
repos <- "http://cran.us.r-project.org"
if(!require(ggplot2)) install.packages("ggplot2", repos = repos)
if(!require(plotly)) install.packages("rgeos", repos = repos)
if(!require(dplyr)) install.packages("rgeos", repos = repos)

# DO NOT CHANGE: Source files, my own codes that we will need
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
source('./auxiliary_functions.R')

# DO NOT CHANGE: URL from where data is extracted
url_raw_data <- paste0("https://raw.githubusercontent.com/",
                       "datadista/datasets/master/COVID%2019/",
                       "nacional_covid19.csv")
url_data_by_age_sex <- paste0("https://raw.githubusercontent.com/",
                              "datadista/datasets/master/COVID%2019/",
                              "nacional_covid19_rango_edad.csv")


########################################
########################################
########################################

##
## PARAMETERS TO BE CHANGED: please, answer the following questions
##

# suscep: do you want to plot in the SIR graphic the population of susceptibles?
#         Note that if suscep = TRUE, the scale could be confused since
#         the amount of susceptibles is a few million. Defaults to FALSE
suscep <- FALSE

# by_sex: do you want to plot the cumulative cases and cumulative deaths by sex?
#         Note that just this data is available by sex. Defaults to TRUE.
by_sex <- TRUE

# plot_vel: do you want to plot the daily growth (%) of active cases, deaths
#           and recovered? Defaults to TRUE.
plot_vel <- TRUE



# Names of variables of the table where the raw data is contained
var_names_raw_data <- c("fecha", "casos", "fallecimientos", "altas",
                        "ingresos_uci", "hospitalizados")

# Names of variables of the table where the data disaggregated by sex and
# age is contained
var_names_sex_age_data <- c("fecha", "casos_confirmados", "fallecidos")

# Demography: total, male, female (July 2019, Spanish Institute of Statistics)
population <- data.frame("total" = 47100396, "male" = 23089389,
                         "female" = 24011007)

##
## NOTE: data from Spanish Health Ministery are collected from the cases
##       reported by the CCAA (autonomous regions) one day before at 20:00.
##       The difference between days could be included late notices.
##
## NOTE: Since 8th April, Spanish Health Ministery does not provide data about
##       admissions and ICU admissions, since some regions were providing
##       cumulative data and others prevalence data.
##

# Plotting some descriptive figures
figures <- desc_analysis_spa_data(url_raw_data, url_data_by_age_sex,
                                  var_names_raw_data, var_names_sex_age_data,
                                  population, suscep = suscep, by_sex = by_sex,
                                  plot_vel = plot_vel)




