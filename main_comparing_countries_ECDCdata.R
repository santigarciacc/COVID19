# Remove all the variables
rm(list = ls())

# DO NOT CHANGE: libraries required: Rcode that we will need
repos <- "http://cran.us.r-project.org"
if(!require(httr)) install.packages("httr", repos = repos)
if(!require(plotly)) install.packages("plotly", repos = repos)
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = repos)
if(!require(readxl)) install.packages("readxl", repos = repos)
if(!require(utils)) install.packages("utils", repos = repos)


# DO NOT CHANGE: Source files, my own codes that we will need
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
source('./aux_functions.R')
source('./countries_by_continents.R')


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

countries <- c("ABW", "FRO", "AND", "VAT", "LKA", "GIB",
               "FJI", "SYC", "SMR", "MSR") 

# LOG: Do you want data in the original scale or in the log scale?
log <- FALSE # If log <- TRUE, cases and deaths are plotted in log scale

# DATES: Since when do you want to analysed?
from_date <- "2019-12-31" # Please, modify with a date in format "YYYY-mm-dd"
to_date <- format(Sys.time(), "%Y-%m-%d") # DO NOT CHANGE
dates <- as.Date(c(from_date, to_date)) # DO NOT CHANGE

# SAVE_LOCAL: Do you want to save in your computer the datasets? TRUE/FALSE
save_local <- FALSE

# N_HAB: Do you want to show all data in a relative way, in terms of
# each n_hab people? For example, each 10.000 people (n_hab = 10000)
n_hab <- 1e2 # If n_hab <- FALSE, deaths or cases are plotted as usual

# ALIGNED: Do you want to align the data, starting all graphics at Day 0,
# defining as the first day in which cumulative cases are greater than
# population * perc_pop (%), or whether do you prefer to align the data fixing
# as Day 0 the first day in which cumulative deaths are greater than
# population * perc_pop (%)
aligned_cases <- TRUE
aligned_deaths <- FALSE # Defaults FALSE if aligned_cases == TRUE
perc_pop <- 0.000005 # 0.0005 % of population
  
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
plot_dev_by_continents <- TRUE # A graphic about the data related to their continents




##########################################
#### FROM HERE, DO NOT CHANGE THE CODE ###
##########################################

#
# Example of using: write
# ECDC_ESP$covid_data # covid19 data of Spain from the selected dates
# ECDC_ESP$country # Country code
# ECDC_ESP$population # Population in the databases are from the World Bank
#
# writting graphics_countries$fig_cum_deaths plots cumulative deaths
#
# If you write "graphics_countries$" (without quotes), and then, press tab key,
# a menu will appear with the different graphics available.


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

for (i in 1:length(ECDC_data$data_by_continents)) {
  
  assign(paste0("ECDC_", ECDC_data$data_by_continents[[i]]$continent),
         ECDC_data$data_by_continents[[i]])
}



#
# Example of using: write
# ECDC_ESP$covid_data # covid19 data of Spain from the selected dates
# ECDC_ESP$country # Country code
# ECDC_ESP$population # Population in the databases are from the World Bank
# ECDC_europe # Data of Europe


##
## DO NOT CHANGE: creating graphics
## Example: writting graphics_countries$fig_cum_deaths plots cumulative deaths
##
graphics_countries <-
  comparative_countries(ECDC_data, log = log, n_hab,
                        aligned_cases = aligned_cases,
                        aligned_deaths = aligned_deaths, perc_pop = perc_pop,
                        plot_cases = plot_cases, plot_deaths = plot_deaths,
                        plot_cum_cases = plot_cum_cases,
                        plot_cum_deaths = plot_cum_deaths,
                        plot_mort_rate = plot_mort_rate,
                        plot_v_cases = plot_v_cases,
                        plot_v_deaths = plot_v_deaths,
                        plot_a_cases = plot_a_cases,
                        plot_a_deaths = plot_a_deaths,
                        plot_dev_by_continents = plot_dev_by_continents)


