# COVID19
**Scripts for describing, analysing and predicting the pandemic COVID19**

The code available is composed of two files, one of them with auxiliary functions (aux_functions.R), which should not change it. Please, **download both files** (aux_functions.R and main_comparing_countries_ECDCdata.R) and **just open the main file (main_comparing_countries_ECDCdata.R)**.

You can execute the whole code just pressing the **"save" button in the R studio, making sure that "source on save" is ticked**. You can also execute in the classical terminal of R by writting (note that "your_path_of_files" is the path of your computer where you have saved the files)

- source('your_path_of_files/main_comparing_countries_ECDCdata.R')

Please, **read the questions asked at the beginning of main_comparing_countries_ECDCdata.R**: logarithmic scale (TRUE/FALSE), save  the files in your local (TRUE/FALSE), countries to be plotted (an array of characters), aligned curves from the "Day 0" of epidemic in each country, among other options.

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




