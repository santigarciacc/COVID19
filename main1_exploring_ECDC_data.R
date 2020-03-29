# Libraries required
library(plotly)
library(RColorBrewer)

# Source files
working_path <- "~/Dropbox/DIVULGACIÓN/COVID19/Códigos R"
setwd(working_path) # Setting working directory to source file location
source('~/Dropbox/DIVULGACIÓN/COVID19/Códigos R/aux_funs_extract_ECDC.R')

##
## PARAMETERS
##
countries <- c("ESP", "ITA", "DEU", "FRA", "CHN",
               "KOR", "USA", "GBR", "IRN") # Countries to be analysed
dates <- as.Date(c("2019-12-31",
                   format(Sys.time(), "%Y-%m-%d"))) # Dates to be analysed
save_local <- FALSE # If TRUE, files are stored in your local directory

# If TRUE, data are aligned: day 0 when the cumulative infected > min_cases
aligned <- TRUE
min_cases <- 100

# If TRUE (and aligned == FALSE), data is not aligned but just the rows with
# the cumulative infected > min_cases are analysed
threshold <- FALSE 

# If some of them is TRUE, the figure is provided.
# inc_inf: daily infected // inc_deats: daily deaths
# cum_inf: cumulative infected // cum_deaths: cumulative deaths
# growth_inf: % daily growth infected // growth_deaths: % daily growth deaths
# rate_mortality: ratio deaths/infected
# vel_growth (just with aligned = TRUE): reference lines when cases x2
# each day, each 2 days, each 3 days and each 4 days
inc_inf <- inc_deaths <- cum_inf <- cum_deaths <- growth_inf <-
  growth_deaths <- mortality_rate <- TRUE


##
## EXTRACTING (AND STORING) FROM THE ECDC WEBSITE
##
ECDC_data <-
  extracting_data_covid19(dates = dates, countries = countries,
                          save_local = save_local, aligned = aligned,
                          min_cases = min_cases)

# Each country is stored in a list named as "ECDC_" + country code
for (i in 1:length(ECDC_data$filter_data)) {
  
  assign(paste0("ECDC_", ECDC_data$filter_countries[i]),
         ECDC_data$filter_data[[i]])
  
}


##
## PLOTTING: GRAPHICS ARE STORED IN FIGURES
## Example: writting figures$fig_cum_deaths plots the figure of cumul. deaths
##
figures <-
  comparative_countries(ECDC_data, threshold = threshold, aligned = aligned,
                        min_cases = min_cases, inc_inf = inc_inf,
                        inc_deaths = inc_deaths, cum_inf = cum_inf,
                        cum_deaths = cum_deaths, growth_inf = growth_inf,
                        growth_deaths = growth_deaths,
                        mortality_rate = mortality_rate)


# Plotting velocity of growth for a single country
rate <- 2 # The cases are duplicated
each_days <- c(2, 3, 4, 5) # The cases are xrate each "each days"
from <- 12 # The first to be compared
fig_vel_growth <- plot_vel_growth(ECDC_ESP, rate = rate, from = from,
                                  each_days = each_days)



