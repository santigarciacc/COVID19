# COVID19
**Scripts for describing, analysing and predicting the pandemic COVID19**

The code available is composed of **three code files and one dataset**:

- **aux_functions.R**: code with auxiliary functions which should not change it. 

- **countries_by_continents.R**: code for classify each country in its continent. Note that Russia has been included in Europe and Turkey in Asia. The dataset with the classifiction is contained in countries_by_continents.R

- **main_comparing_countries_ECDCdata.R**: code for extracting, filtering and plotting the data. **IMPORTANT**: this code is the only piece of code that the non-expertised user should execute and open.


**Download all files** in the same folder of your computer and **just open the main file (main_comparing_countries_ECDCdata.R)**


![Head of code. Source on save must be ticked](https://github.com/JavierAlvarezLiebana/COVID19/blob/master/head_code_source_on_save.jpg
)

Note that at the beginning of the main code [main](https://github.com/JavierAlvarezLiebana/COVID19/blob/master/main_comparing_countries_ECDCdata.R), all **variables will be removed** and all packages will be attached. You can execute the whole code just pressing the **save button** in the R studio, making sure that the **option source on save is previously ticked**. You can also execute in the classical terminal of R by writting (note that "your_path_of_files" is the path of your computer where you have saved the files)

```R
source('your_path_of_files/main_comparing_countries_ECDCdata.R')
```

Please, **before executing, read the questions asked** at the beginning of [main](https://github.com/JavierAlvarezLiebana/COVID19/blob/master/main_comparing_countries_ECDCdata.R): 

- **countries**: introduce the **code of the countries** that you want to plot (maximum 11 at the same time). Check the available codes in [countries_by_continents](https://github.com/JavierAlvarezLiebana/COVID19/blob/master/countries_by_cont.RData)

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

- **aligned_deaths**:  do you want to **align the data** `TRUE`, starting all graphics at **Day 0 of pandemic** defining as the first day in which **cumulative deaths are greater than population * perc_pop**, where `perc_pop` denotes a percentage of the population, or not `FALSE`?

```R
aligned_deaths <- FALSE
perc_pop <- 0.000001 # 0.0001 % of population of each country
```

**Examples of using (after executing the code)**:

If you write in the console

- ECDC_data$raw_data: raw data (all countries, all dates).

- ECDC_ESP$covid_data: covid19 data of Spain, from the ECDC database, between the selected dates, updated just in time of the execution.

- ECDC_data$all_countries: all countries (their codes) included in the database.

- ECDC_data$filter_countries: asked country codes

- ECDC_data$filter_data[[i]]: filtered data (by countris and dates) of the i-th country asked (included in ECDC_data$filter_countries).

- ECDC_ESP$country and ECDC_ITA$country # Country codes (in this case "ESP" and "ITA", resp.)

- ECDC_ESP$population: population in January 2019 from the World Bank database

- graphics_countries$fig_cum_deaths: plots cumulative deaths

- graphics_countries$fig_vel_cases: plots the % daily growth of cases 

- graphics_countries$ + tab key: a menu will appear with the different graphics available.




