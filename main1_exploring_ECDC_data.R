# Remove all the variables
rm(list = ls())

# DO NOT CHANGE: libraries required: Rcode that we will need
repos <- "http://cran.us.r-project.org"
if(!require(httr)) install.packages("httr", repos = repos)
if(!require(plotly)) install.packages("plotly", repos = repos)
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = repos)
if(!require(readxl)) install.packages("readxl", repos = repos)

# DO NOT CHANGE: Source files, my own codes that we will need
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
source('./aux_functions.R')


########################################
########################################
########################################


##
## PARAMETERS TO BE CHANGED: please, answer the following questions
##

# COUNTRIES: Which countries do you want to be compared??
# Codes country available (among others): AFG ARG ARM AUS AUT BEL BOL BRA CAN
# CHL CHN COL CUB CZE DNK ECU EGY SLV EST SWZ ETH FIN FRA DEU GRC ISL IND IRN
# IRL ITA JPN LUX MEX NLD NOR PER POL PRT RUS SEN KOR ESP SWE TUR GBR USA VEN
countries <- c("ESP", "ITA", "USA", "MEX","ECU", "ARG", "BRA",
               "CHN") 

# LOG_10: Do you want data in the original scle or in the log_10 scale?
log_10 <- FALSE # If log_10 <- TRUE, cases and deaths are plotted in log_10 scale

# DATES: Since when do you want to analysed?
from_date <- "2019-12-31" # Please, modify with a date in format "YYYY-mm-dd"
to_date <- format(Sys.time(), "%Y-%m-%d") # DO NOT CHANGE
dates <- as.Date(c(from_date, to_date)) # DO NOT CHANGE

# SAVE_LOCAL: Do you want to save in your computer the datasets? TRUE/FALSE
save_local <- TRUE

# N_HAB: Do you want to show all data in a relative way, in terms of
# each n_hab people? For example, deaths or cases each 10.000 people
n_hab <- FALSE # If n_hab <- FALSE, deaths or cases are plotted as usual

# ALIGNED: Do you want to align the data, starting all graphics at Day 0,
# defining as the first day in which cumulative cases are greater than
# 100, or whether do you prefer to align the data fixing as Day 0 the 
# first day in which cumulative deaths are greater than 10
aligned_cases <- FALSE
aligned_deaths <- FALSE # Defaults FALSE if aligned_cases == TRUE

# VEL_MULT: As long as aligned_cases == TRUE, do you want a graphic for
# checking the velocity of the growth of cases? Lines concerning what would
# happen if cases are multiplied by "rate" each "each_days" (example: if
# rate = 2 and each_days = c(2, 3, 4), we measure what would happen if cases are
# duplicated (x2, since rate = 2) each 2 days, each 3 days and each 4 days).
vel_mult <- FALSE
rate <- 2 # Measure would happen if cases are x2 each "each days"
each_days <- c(2, 3, 4, 5) # Measure would happen if cases are x2 each 2, 3, 4 and 5 days
from <- 12 # DO NOT CHANGE: the first past day to be compared

# PLOT_CASES, PLOT_DEATHS, PLOT_CUM_CASES, PLOT_CUM_DEATHS, PLOT_MORT_RATE
plot_cases <- TRUE # A graphic about the daily cases? TRUE/FALSE
plot_deaths <- TRUE # A graphic about the daily deaths? TRUE/FALSE
plot_cum_cases <- TRUE # A graphic about the cum. cases? TRUE/FALSE
plot_cum_deaths <- TRUE # A graphic about the cum. deaths? TRUE/FALSE
plot_mort_rate <- TRUE # A graphic about the mortality rate (cum deaths / cum cases)? TRUE/FALSE

# PLOT_V_CASES, PLOT_V_DEATHS, PLOT_A_CASES, PLOT_A_DEATHS
plot_v_cases <- TRUE # A graphic about the % growth (velocity) of cases? TRUE/FALSE
plot_v_deaths <- TRUE # A graphic about the % growth (velocity) of deaths? TRUE/FALSE
plot_a_cases <- TRUE # A graphic about the % growth of velocity of cases (acceleration)? TRUE/FALSE
plot_a_deaths <- TRUE # A graphic about the % growth of velocity of deaths (acceleration)? TRUE/FALSE


##########################################
#### FROM HERE, DO NOT CHANGE THE CODE ###
##########################################


##
## DO NOT CHANGE: extracting from the ECDC (European Centre for Disease
##                Prevention and Control) or the  website
## 
## A complete data set ECDC_data is created
##  * ECDC_data$raw_data: the raw data directly downloaded
##  * ECDC_data$all_countries: all countries available
##  * ECDC_data$filter_countries: just the countries selected
##  * ECDC_data$filter_data[[i]]: just data from i-th country and dates selected
##
ECDC_data <-
  extracting_ECDC_covid19(dates = dates, countries = countries,
                          save_local = save_local)
# Each country is also stored in a list named as "ECDC_" + country code
for (i in 1:length(ECDC_data$filter_data)) {
  
  assign(paste0("ECDC_", ECDC_data$filter_countries[i]),
         ECDC_data$filter_data[[i]])
  
}

##
## Example of using:
##
# ECDC_ESP$covid_data # covid19 data of Spain from the selected dates
# ECDC_ESP$country # Country code
# ECDC_ESP$population # Population in the databases are from the World Bank



##
## DO NOT CHANGE: creating graphics
## Example: writting graphics_countries$fig_cum_deaths plots cumulative deaths
##
graphics_countries <-
  comparative_countries(ECDC_data, log_10 = log_10,
                        n_hab, aligned_cases = aligned_cases,
                        aligned_deaths = aligned_deaths,
                        plot_cases = plot_cases, plot_deaths = plot_deaths,
                        plot_cum_cases = plot_cum_cases,
                        plot_cum_deaths = plot_cum_deaths,
                        plot_mort_rate = plot_mort_rate, vel_mult = vel_mult,
                        plot_v_cases = plot_v_cases,
                        plot_v_deaths = plot_v_deaths,
                        plot_a_cases = plot_a_cases,
                        plot_a_deaths = plot_a_deaths,
                        rate = rate, each_days = each_days, from = from)


