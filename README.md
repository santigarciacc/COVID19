# COVID19

[cran]: https://www.r-pkg.org/badges/version/zeallot "green means go!"
![alt text][cran]


## PART I: Scripts for describing and comparing between countries data from pandemic COVID19. Data extracted from European Centre for Disease Prevention and Control repository (see [Part II](https://github.com/JavierAlvarezLiebana/COVID19/blob/master/README.md#part-ii-scripts-for-exploring-and-predicting-data-from-pandemic-covid19-in-spain) below for a more extended analysis of Spanish data)

Data for exploring and comparing between countries is obtained (and **daily automatically updated**) from [repository of ECDC](https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide) (repository of **European Centre for Disease Prevention and Control**). Note that data from [repository of ECDC](https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide) is daily updated but a day later


### Description of files

The code available is composed of **three code files and two datasets**:

- **aux_functions.R**: code with auxiliary functions which should not change it. 

- **countries_by_continents.R**: code for classify each country in its continent. Note that Russia has been included in Europe and Turkey in Asia. The dataset with the classification is contained in [countries_by_cont](https://github.com/JavierAlvarezLiebana/COVID19/blob/master/datasets/countries_by_cont.RData)

- **main_comparing_countries_ECDCdata.R**: code for extracting, filtering and plotting the data. **IMPORTANT**: this code is the only piece of code that the non-expertised user should execute and open.

- **countries_by_cont.RData**: dataset with countries classified by continents (see  [countries_by_cont](https://github.com/JavierAlvarezLiebana/COVID19/blob/master/datasets/countries_by_cont.RData))

- **updated_data.RData**: raw dataset directly extracted from  [repository of ECDC]  (see  [updated_data](https://github.com/JavierAlvarezLiebana/COVID19/blob/master/datasets/updated_data.RData))


### Installation (please, check before question to be asked below):

#### Option 1: R studio (recommended):

**Download all files (in a zip, then unzip)** of [repository](https://github.com/JavierAlvarezLiebana/COVID19/tree/master) (no matter the folder where you save it) and then **just open** [main file](https://github.com/JavierAlvarezLiebana/COVID19/blob/master/main_comparing_countries_ECDCdata.R)

![Head of code. Source on save must be ticked](https://github.com/JavierAlvarezLiebana/COVID19/blob/master/figures/head_code_source_on_save.jpg)

Note that at the beginning of the [main file](https://github.com/JavierAlvarezLiebana/COVID19/blob/master/main_comparing_countries_ECDCdata.R), all **variables will be removed** and all packages required will be attached. You can execute the whole code just pressing the **save button** (the save button which appears in the previous picture) in the R studio, making sure that the **option** `source on save` (next to the save button) is **previously ticked**.

#### Option 2: classical R terminal:

You can also execute in the classical terminal of R by writting (note that `your_path_of_files` should b the path of your computer where you have saved the files)
 
```R
source('your_path_of_files/main_comparing_countries_ECDCdata.R')
```


### Questions to be asked before executing:

Please, **before executing, read the questions asked** at the beginning of the [main file](https://github.com/JavierAlvarezLiebana/COVID19/blob/master/main_comparing_countries_ECDCdata.R): 

- **countries**: introduce the **code of the countries** that you want to plot (maximum 11 at the same time). Check the available codes in [code_countries_by_cont](https://github.com/JavierAlvarezLiebana/COVID19/blob/master/countries_by_continents.R) and the dataset in [countries_by_cont] for searching **ISO code countries**.

```R
countries <- c("ESP", "ITA", "FRA", "DEU", "PRT", "GBR", "USA",
               "CAN", "MEX", "CHN", "NLD")
```

- **log**: do you want data in the **original scale** `FALSE` or in the **log scale** `TRUE`?

```R
log <- FALSE # If log <- TRUE, cases and deaths are plotted in log_10 scale
```

- **from_date**: since **when** do you want to analysed? For example, since `from_date <- "2019-12-31" `

```R
from_date <- "2019-12-31" # Please, modify with a date in format "YYYY-mm-dd"
to_date <- format(Sys.time(), "%Y-%m-%d") # DO NOT CHANGE
dates <- as.Date(c(from_date, to_date)) # DO NOT CHANGE
```

- **save_local**: do you want to **save in your computer** the datasets `TRUE`or not `FALSE`?

```R
save_local <- TRUE
```

- **n_hab** do you want to show all data in a **relative way, in terms of each** `n_hab` **people**? For example, `n_hab <- 10000` would be for each **10.000 people**. Otherwise, please fix `n_hab <- FALSE`.

```R
n_hab <- 1e6 # If n_hab <- FALSE, deaths or cases are plotted as usual
```

- **aligned_cases**:  do you want to **align the data** `TRUE`, starting all graphics at **Day 0 of pandemic** defining as the first day in which **cumulative cases are greater than population * perc_pop**, where `perc_pop` denotes a percentage of the population, or not `FALSE`?

```R
aligned_cases <- TRUE
perc_pop <- 0.000005 # 0.0005 % of population of each country
```

#### Flags for plotting:

Which graphics do you want to be plotted?

- **plot_cases**: do you want a graphic plotting the **daily cases** `TRUE` or not `FALSE`?
- **plot_deaths**: do you want a graphic plotting the **daily deaths** `TRUE` or not `FALSE`?
- **plot_cum_cases**: do you want a graphic plotting the **cum cases** `TRUE` or not `FALSE`?
- **plot_cum_deaths**: do you want a graphic plotting the **cum deaths** `TRUE` or not `FALSE`?
- **plot_morth_rate**: do you want a graphic plotting the **daily mortality rate** (cum deaths / cum cases) `TRUE` or not `FALSE`?
- **plot_v_cases**: do you want a graphic plotting the **% growth (velocity) of cases** `TRUE` or not `FALSE`?
- **plot_v_deaths**: do you want a graphic plotting the **% growth (velocity) of deaths** `TRUE` or not `FALSE`?
- **plot_a_cases**: do you want a graphic plotting the **% growth of velocity (acceleration) of cases** `TRUE` or not `FALSE`?
- **plot_a_deaths**: do you want a graphic plotting the **% growth of velocity (acceleration) of deaths** `TRUE` or not `FALSE`?
- **plot_dev_by_continents**: do you want a graphic plotting the **cases and deaths (and their cumulatives) in comparison with their continets** (by habitants) `TRUE` or not `FALSE`?

```R
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
```

### Examples of using the datasets (after executing the code):

#### Raw ECDC data:

Loading the **dataset (data frame) with the raw data** directly from the [repository of ECDC]. Note that the data is chronologically sorted, the newest on top:

```R
ECDC_data$raw_data
```


Loading the **set of code countries** (a vector) for all available countries:

```R
ECDC_data$all_countries
```

Loading the **set of code countries** (a vector) for the **filtered** countries asked before in the `countries` variable:

```R
ECDC_data$filter_countries
```

#### Filtered ECDC data by countries:

Loading the **dataset (data frame) with the raw data** directly from the [repository of ECDC], filtered by countries in `countries` and dates in `dates`. Note that the data is chronologically sorted, the newest on top. For using the i-th country indicated in `countries` variable, please write


```R
ECDC_data$filter_data[[i]]$country # Code country for the i-th country asked in the variable "countries"
ECDC_data$filter_data[[i]]$continent # Continent for the i-th country asked in the variable "countries"
ECDC_data$filter_data[[i]]$population # Population (2018) for the i-th country asked in the variable "countries"
ECDC_data$filter_data[[i]]$covid_data # covid19 data for the i-th country asked in the variable "countries"
```

**Files** (data frame) for **each country** have been also created:

```R
ECDC_ESP # Data for Spain
ECDC_ESP$country # Code country of Spain
ECDC_ESP$continent # Continent of Spain 
ECDC_ESP$population # Population of Spain
ECDC_ESP$covid_data # Covid19 data of Spain

ECDC_ESP$covid_data$date # Dates
ECDC_ESP$covid_data$cases # Daily cases
ECDC_ESP$covid_data$cum_cases # Cumulative cases
ECDC_ESP$covid_data$deaths # Daily deaths
ECDC_ESP$covid_data$cum_deaths # Cumulative deaths
ECDC_ESP$covid_data$vel_cases # % daily growth of cases
ECDC_ESP$covid_data$vel_deaths # % daily growth of deaths
ECDC_ESP$covid_data$acc_cases # % daily growth of growth (acceleration) of cases
ECDC_ESP$covid_data$acc_deaths # % daily growth of growth (acceleration) of deaths
ECDC_ESP$covid_data$mort_rate # Daily mortality rate

# Relative errors respect to the continent data: (country - continent)/continent
ECDC_ESP$covid_data$dev_cont_cases 
ECDC_ESP$covid_data$dev_cont_cum_cases
ECDC_ESP$covid_data$dev_cont_deaths
ECDC_ESP$covid_data$dev_cont_cum_deaths

```



#### Raw ECDC data by continents:

Loading the **dataset (data frame) with the raw data** directly from [repository of ECDC], but grouped by continents Note that the data is chronologically sorted, the newest on top:

```R
ECDC_data$data_by_continents[[1]] # Europe

ECDC_data$data_by_continents[[1]]$population # Population of Europe
ECDC_data$data_by_continents[[1]]$continent # Name of continent
ECDC_data$data_by_continents[[1]]$covid_data # Covid19 data of Europe
```

```R
ECDC_data$data_by_continents[[2]] # Asia

ECDC_data$data_by_continents[[2]]$population # Population of Asia
ECDC_data$data_by_continents[[2]]$continent # Name of continent
ECDC_data$data_by_continents[[2]]$covid_data # Covid19 data of Asia
```

```R
ECDC_data$data_by_continents[[3]] # Africa

ECDC_data$data_by_continents[[3]]$population # Population of Africa
ECDC_data$data_by_continents[[3]]$continent # Name of continent
ECDC_data$data_by_continents[[3]]$covid_data # Covid19 data of Africa
```

```R
ECDC_data$data_by_continents[[4]] # Oceania

ECDC_data$data_by_continents[[4]]$population # Population of Oceania
ECDC_data$data_by_continents[[4]]$continent # Name of continent
ECDC_data$data_by_continents[[4]]$covid_data # Covid19 data of Oceania
```

```R
ECDC_data$data_by_continents[[5]] # South America

ECDC_data$data_by_continents[[5]]$population # Population of South America
ECDC_data$data_by_continents[[5]]$continent # Name of continent
ECDC_data$data_by_continents[[5]]$covid_data # Covid19 data of South America
```

```R
ECDC_data$data_by_continents[[6]] # North and central America (and Caribe region)

ECDC_data$data_by_continents[[6]]$population # Population of North and central America (and Caribe region)
ECDC_data$data_by_continents[[6]]$continent # Name of continent
ECDC_data$data_by_continents[[6]]$covid_data # Covid19 data of North and central America (and Caribe region)
```


### Examples of using the graphics (after executing the code):

Showing the **graphics asked** (note that if some of the flags were `FALSE`, then the variable `graphics_countries`will be `NULL`for these graphic):

```R
graphics_countries$fig_cases # Plotting the daily cases for each selected country
```

Daily cases aligned by cases per each 1 millon habitants, 4th April 2020:

![Daily cases aligned by cases per each 1 millon habitants, 4th April 2020](https://github.com/JavierAlvarezLiebana/COVID19/blob/master/figures/daily_cases.jpg)


```R
graphics_countries$fig_deaths # Plotting the daily deaths for each selected country
```

Daily deaths aligned by cases per each 1 millon habitants, 4th April 2020:

![Daily deaths aligned by cases per each 1 millon habitants, 4th April 2020](https://github.com/JavierAlvarezLiebana/COVID19/blob/master/figures/daily_deaths.jpg)


```R
graphics_countries$fig_cum_cases # Plotting the cumulative cases for each selected country
```

Cum cases aligned by cases per each 1 millon habitants, 4th April 2020:

![Cum cases aligned by cases per each 1 millon habitants, 4th April 2020](https://github.com/JavierAlvarezLiebana/COVID19/blob/master/figures/cum_cases.jpg)


**Note that each figure is interactive and it can be enlarged just selecting the zoom area**

![Cum cases aligned by cases per each 1 millon habitants, 4th April 2020](https://github.com/JavierAlvarezLiebana/COVID19/blob/master/figures/zoom.jpg)




```R
graphics_countries$fig_cum_deaths # Plotting the cumulative deaths for each selected country
```

```R
graphics_countries$fig_vel_cases # Plotting the velocity of cases for each selected country
```

```R
graphics_countries$fig_vel_deaths # Plotting the velocity of deaths for each selected country
```

```R
graphics_countries$fig_acc_cases # Plotting the acceleration of cases for each selected country
```

```R
graphics_countries$fig_acc_deaths # Plotting the acceleration of deaths for each selected country
```

```R
graphics_countries$fig_mort_rate # Plotting the daily mortality rate for each selected country
```

```R
graphics_countries$fig_dev_cont_cases # Plotting relative deviations (resp. to their continents) of cases for each selected country
```

```R
graphics_countries$fig_dev_cont_deaths # Plotting relative deviations (resp. to their continents) of deaths for each selected country
```

```R
graphics_countries$fig_dev_cont_cum_cases # Plotting relative deviations (resp. to their continents) of cum cases for each selected country
```

```R
graphics_countries$fig_dev_cont_cum_deaths # Plotting relative deviations (resp. to their continents) of deaths for each selected country
```

**IMPORTANT**: remember that data from [repository of ECDC] is updated a day later, then you are plotting the data from yesterday.


## PART II: Scripts for exploring and predicting data from pandemic COVID19 in Spain
