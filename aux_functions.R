library(utils)
library(httr)

filtering_data_covid19 <-
  function(raw_data, dates = as.Date(c("2019-12-31", max_date)),
           countries = c("ESP", "ITA", "FRA", "CHN", "KOR", "USA", "GBR"),
           data_by_continents, countries_by_cont) {
    
    # Check if all countries are allowed
    if (!all(countries %in% raw_data$countryterritoryCode)) {
      
      stop(paste("Sorry, some of the country codes is wrong",
                 "or the country is not currently available"))
      
    }
 
    # Filter the data according to dates and countries selected
    filtered_data <-
      raw_data[(raw_data$countryterritoryCode %in% countries) &
                  (as.Date(raw_data$dateRep, "%d-%m-%Y", tz = "UTC") %in%
                                            seq(dates[1], dates[2], by = 1)), ]

    # Provide a list of data frames for each filtered country
    output_list <- list()
    for(c in 1:length(countries)) {
      
      # Indexes for the first row of each filtered country
      country_indexes <- which(raw_data$countryterritoryCode == countries[c])
      
      # Cum_today / cum_yesterday (for each day)
      aux_cum <- cumsum(rev(raw_data$cases[country_indexes]))[-1] /
        rev(rev(cumsum(rev(raw_data$cases[country_indexes])))[-1])
      aux_cum_deaths <- cumsum(rev(raw_data$deaths[country_indexes]))[-1] /
        rev(rev(cumsum(rev(raw_data$deaths[country_indexes])))[-1])
      
      output_list[[c]] <-
        list("covid_data" =
               data.frame("date" = raw_data$dateRep[country_indexes],
                          "cases" = raw_data$cases[country_indexes],
                          "cum_cases" =
                            rev(cumsum(rev(raw_data$cases[country_indexes]))),
                          "deaths" = raw_data$deaths[country_indexes],
                          "cum_deaths" =
                            rev(cumsum(rev(raw_data$deaths[country_indexes]))),
                          "vel_cases" =
                            rev(c(0, ifelse(is.nan(aux_cum) |
                                              is.infinite(aux_cum), 0,
                                            aux_cum - 1))), # Expressed as %
                          "vel_deaths" =
                            rev(c(0, ifelse(is.nan(aux_cum_deaths)  |
                                              is.infinite(aux_cum_deaths), 0,
                                            aux_cum_deaths - 1)))),
             "country" = countries[c],
             "continent" =
               names(countries_by_cont)[mapply(countries[c],
                                               FUN = "%in%", countries_by_cont)],
             "population" = raw_data$popData2018[country_indexes[1]])
      
      # Mortality rate
      output_list[[c]]$covid_data$mort_rate <-
        output_list[[c]]$covid_data$cum_deaths /
        output_list[[c]]$covid_data$cum_cases
        
      # Providing the velocity of the growth (acceleration) of cases
      aux_acc_cases <- rev(rev(output_list[[c]]$covid_data$vel_cases)[-1]) /
        output_list[[c]]$covid_data$vel_cases[-1]
      output_list[[c]]$covid_data$acc_cases <-
        c(ifelse(is.nan(aux_acc_cases) | is.infinite(aux_acc_cases), 0,
                 aux_acc_cases - 1), 0)
               
      # Providing the velocity of the growth (acceleration) of deaths
      aux_acc_deaths <- rev(rev(output_list[[c]]$covid_data$vel_deaths)[-1]) /
        output_list[[c]]$covid_data$vel_deaths[-1]
      output_list[[c]]$covid_data$acc_deaths <-
        c(ifelse(is.nan(aux_acc_deaths) | is.infinite(aux_acc_cases), 0,
                 aux_acc_deaths - 1), 0)
      
      ##
      ## Data comparing with its continent
      ## 
      idx_continent <-
        which(mapply(countries[c], FUN = "%in%", countries_by_cont))
      
      # Cases
      cases_by_hab <- output_list[[c]]$covid_data$cases /
        output_list[[c]]$population
      cases_by_cont <-
        data_by_continents[[idx_continent]]$covid_data$cases[
          1:length(country_indexes)] /
        data_by_continents[[idx_continent]]$population 
      
      # Cum cases
      cum_cases_by_hab <- output_list[[c]]$covid_data$cum_cases /
        output_list[[c]]$population
      cum_cases_by_cont <-
        data_by_continents[[idx_continent]]$covid_data$cum_cases[
          1:length(country_indexes)] /
        data_by_continents[[idx_continent]]$population 
      
      # Deaths
      deaths_by_hab <- output_list[[c]]$covid_data$deaths /
        output_list[[c]]$population
      deaths_by_cont <-
        data_by_continents[[idx_continent]]$covid_data$deaths[
          1:length(country_indexes)] /
        data_by_continents[[idx_continent]]$population 
      
      # Cum deaths
      cum_deaths_by_hab <- output_list[[c]]$covid_data$cum_deaths /
        output_list[[c]]$population
      cum_deaths_by_cont <-
        data_by_continents[[idx_continent]]$covid_data$cum_deaths[
          1:length(country_indexes)] /
        data_by_continents[[idx_continent]]$population
      
      
      output_list[[c]]$covid_data$dev_cont_cases <-
        ifelse(cases_by_cont == 0, 0, 
               (cases_by_hab - cases_by_cont) / cases_by_cont)
      output_list[[c]]$covid_data$dev_cont_cum_cases <-
        ifelse(cum_cases_by_cont == 0, 0, 
               (cum_cases_by_hab - cum_cases_by_cont) / cum_cases_by_cont)
      output_list[[c]]$covid_data$dev_cont_deaths <-
        ifelse(deaths_by_cont == 0, 0, 
               (deaths_by_hab - deaths_by_cont) / deaths_by_cont)
      output_list[[c]]$covid_data$dev_cont_cum_deaths <-
        ifelse(cum_deaths_by_cont == 0, 0, 
               (cum_deaths_by_hab - cum_deaths_by_cont) / cum_deaths_by_cont)
      
    }
    
    # Output
    return(output_list)
  }


extracting_ECDC_covid19 <-
  function(source = paste0("https://www.ecdc.europa.eu/sites/",
                           "default/files/documents/",
                           "COVID-19-geographic-disbtribution-worldwide-",
                           format(Sys.time(), "%Y-%m-%d"), ".xlsx"),
           file_format = ".xlsx",
           http_auth = authenticate(":", ":", type = "ntlm"),
           dates = as.Date(c("2019-12-31", format(Sys.time(), "%Y-%m-%d"))),
           countries = c("ESP", "ITA", "DEU", "FRA", "CHN", "KOR", "USA",
                         "GBR", "IRN"), save_local = FALSE) {
    
    # Downloading the dataset from the url to our local as a temporary file
    GET(source, http_auth, write_disk(temp_file <-
                                     tempfile(fileext = file_format)))

    # After downloading, we convert the dataset into a data frame
    raw_data <- read_xlsx(temp_file)

    # We extract the list of countries and territories included in the dataset
    all_lands <- na.omit(unique(raw_data$countryterritoryCode))

    ##
    ## GLOBAL DATA: summary of global cases and deaths WITHOUT filtering
    ##
    
    # Array of unique dates (unique provided unsorted dates)
    seq_dates <- sort(unique(raw_data$dateRep), decreasing = TRUE)
    
    # A matrix of length(raw_data$dateRep) x length(seq_dates) with booleans
    # indicated, for each date in "seq_dates" (by columns), which indexes (rows)
    # in the raw_data data.frame are associated with.
    indexes_by_dates <- sapply(seq_dates, FUN = "==", raw_data$dateRep)
    
    # Global cases and deaths
    global_cases <- colSums(matrix(rep(raw_data$cases, length(seq_dates)),
                                   ncol = length(seq_dates)) * indexes_by_dates)
    global_deaths <- colSums(matrix(rep(raw_data$deaths, length(seq_dates)),
                                    ncol = length(seq_dates)) * indexes_by_dates)
    
    global_data <- data.frame("date" = seq_dates,
                              "cases" = global_cases,
                              "cum_cases" = rev(cumsum(rev(global_cases))),
                              "deaths" = global_deaths,
                              "cum_deaths" = rev(cumsum(rev(global_deaths))),
                              "mort_rate" = rev(cumsum(rev(global_deaths))) /
                                rev(cumsum(rev(global_cases))))
    
    # Computing the indexes of the first row for each country
    indexes_countries <-
      sapply(all_lands, FUN = "==", raw_data$countryterritoryCode)
    first_indexes_countries <- 
      unlist(lapply(apply(indexes_countries, FUN = "which", MARGIN = 2),
                    FUN = function(data) {return(data[1])}))
    
    # Global population
    global_data <-
      list("global_data" = global_data,
           "global_population" = 
             sum(na.omit(raw_data$popData2018[first_indexes_countries])))
    
    
    ##
    ## DATA BY CONTINENTS: summary WITHOUT filtering by continents
    ##
    
    load("countries_by_cont.RData")
    data_by_continents <- list()
    for (i in 1:length(countries_by_cont)) {
      
      # Computing the indexes of the first row for each country
      idx_countries <-
        sapply(countries_by_cont[[i]], FUN = "==",
               raw_data$countryterritoryCode)
      first_indexes_countries <- 
        unlist(lapply(apply(idx_countries, FUN = "which", MARGIN = 2),
                      FUN = function(data) {return(data[1])}))
      
      # Population of the continent
      population <- sum(na.omit(raw_data$popData2018[first_indexes_countries]))
      
      # Filtering data by continent
      filter_data_continent <- 
        raw_data[(raw_data$countryterritoryCode %in% countries_by_cont[[i]]), ]
      
      # Array of unique dates (unique provided unsorted dates)
      seq_dates <- sort(unique(filter_data_continent$dateRep),
                        decreasing = TRUE)
      
      # A matrix of length(raw_data$dateRep) x length(seq_dates) with booleans
      # indicated, for each date in "seq_dates" (by columns), which indexes (rows)
      # in the raw_data data.frame are associated with.
      indexes_by_dates <- sapply(seq_dates, FUN = "==",
                                 filter_data_continent$dateRep)
      
      # Cases and deaths
      cases <-
        colSums(matrix(rep(filter_data_continent$cases, length(seq_dates)),
                       ncol = length(seq_dates)) * indexes_by_dates)
      deaths <-
        colSums(matrix(rep(filter_data_continent$deaths, length(seq_dates)),
                       ncol = length(seq_dates)) * indexes_by_dates)
      
      continent_data <- data.frame("date" = seq_dates, "cases" = cases,
                                   "cum_cases" = rev(cumsum(rev(cases))),
                                   "deaths" = deaths,
                                   "cum_deaths" = rev(cumsum(rev(deaths))),
                                   "mort_rate" = rev(cumsum(rev(deaths))) /
                                     rev(cumsum(rev(cases))))
      continent_data$mort_rate <- replace(continent_data$mort_rate,
                                          is.na(continent_data$mort_rate), 0)
      
      # Providing data.frame
      data_by_continents[[i]] <-
        list("covid_data" = continent_data,
             "continent" = names(countries_by_cont)[i],
             "population" = population)

    }
    
    # Just the data of the countries and between dates provided
    filter_data <-
      filtering_data_covid19(raw_data, dates = dates,
                             countries = countries,
                             data_by_continents = data_by_continents,
                             countries_by_cont = countries_by_cont)
    
    if (save_local) {
      
      # Save raw and filter DATA
      today <- as.character(as.Date(format(Sys.time(), "%Y-%m-%d")) - 1)
      save(raw_data, file = paste0("raw_ECDC_data_", today, ".RData"))
      save(filter_data, file = paste0("filtered_ECDC_data_", today, ".RData"))
      
    }
    
    


    # Output
    return(list("raw_data" = raw_data, "filter_data" = filter_data,
                "global_summary" = global_data,
                "data_by_continents" = data_by_continents,
                "all_countries" = all_lands,
                "filter_countries" = countries))
    
}







##
## PLOTS
##

comparative_countries <- function(data, log = FALSE, n_hab = 1,
                                  aligned_cases = FALSE, aligned_deaths = FALSE,
                                  perc_pop = 0.000005,
                                  plot_cases = TRUE, plot_deaths = TRUE,
                                  plot_cum_cases = TRUE, plot_cum_deaths = TRUE,
                                  plot_mort_rate = TRUE,
                                  vel_mult = FALSE, plot_v_cases = TRUE,
                                  plot_v_deaths = TRUE, plot_a_cases = TRUE,
                                  plot_a_deaths = TRUE,
                                  plot_dev_by_continents = TRUE) {
  
  # Number of countries to be compared and number of variables (column)
  n_countries <- length(data$filter_countries)
  
  # Converting to log scale and aligning the curves
  rel_pop <- rep(0, n_countries)
  for (i in 1:n_countries) {
 
    # If log == TRUE, we convert all data to logarithmic scale
    if (log) {
      
      data$filter_data[[i]]$covid_data[ , 2:5] <-
        lapply(log(data$filter_data[[i]]$covid_data[, 2:5]),
               FUN = function(x) replace(x, is.infinite(x), 0))
    }

    if (aligned_cases) {
      
      data$filter_data[[i]]$covid_data <-
        data$filter_data[[i]]$covid_data[
          data$filter_data[[i]]$covid_data$cum_cases >
            data$filter_data[[i]]$population * perc_pop, ]
      
      data$filter_data[[i]]$covid_data$date <-
        rev(0:(length(data$filter_data[[i]]$covid_data$date) - 1))
      
    }
      
    if (aligned_deaths) {
      
      data$filter_data[[i]]$covid_data <-
        data$filter_data[[i]]$covid_data[
          data$filter_data[[i]]$covid_data$cum_deaths >
            data$filter_data[[i]]$population * perc_pop, ]
      
      data$filter_data[[i]]$covid_data$date <-
        rev(0:(length(data$filter_data[[i]]$covid_data$date) - 1))
      
    }
    
    # Relative population for each country
    rel_pop[i] <- ifelse(is.logical(n_hab), 1,
                         data$filter_data[[i]]$population / n_hab)
  }

  # Markers for then plotting
  plot_markers <- c("circle-open", "circle-open-dot", "square-open",
                    "square-open-dot", "diamond-open", "diamond-open-dot",
                    "x-open", "x-open-dot",
                    "triangle-up-open", "triangle-up-dot",
                    "triangle-up-open-dot", "triangle-down-open",
                    "triangle-down-open-dot", "triangle-left-open",
                    "triangle-left-open-dot", "triangle-right-open",
                    "triangle-right-open-dot",
                    "triangle-ne-open", "triangle-ne-open-dot",
                    "triangle-se-open")
  
  # Number of countries should be at most the numbers of different markers
  if (length(plot_markers) < length(data$filter_countries)) {
    
    data$filter_countries[(max_countries + 1):
                            length(data$filter_countries)] <- NULL
    messsage(paste("The number of countries exceeds the maximum amount",
                   "to be plotted: just the data corresponding to the first",
                   max_countries, "countries will be plotted"))
    
  }
  
  # We select random (different between them) markers between the sample
  rand_markers <- sample(plot_markers, length(data$filter_countries),
                         replace = FALSE)
  
  # Defining the number of cases to be considered and dates allowed
  n_cases <- rep(0, length(data$filter_data))
  dates_allowed <- rep(ifelse(aligned_cases | aligned_deaths, 0,
                              as.Date("2019-12-31", '%Y-%m-%d')),
                       length(data$filter_data))
  
  len_max <- 0
  for (i in 1:length(data$filter_data)) {
    
    # If aligned_cases or aligned_deaths are TRUE, we just consider the cases
    # when the cumulative cases (deaths, resp.) is greater than 100 (10, resp.)
    n_cases[i] <-
      ifelse(aligned_cases,
             sum(data$filter_data[[i]]$covid_data$cum_cases >
                   perc_pop * data$filter_data[[i]]$population),
                         ifelse(aligned_deaths,
                                sum(data$filter_data[[i]]$covid_data$cum_deaths >
                                      perc_pop * data$filter_data[[i]]$population),
                                length(data$filter_data[[i]]$covid_data$cum_cases)))
    
    # Dates from where we will start to plot
    dates_allowed[i] <-
      ifelse(aligned_cases | aligned_deaths, 0,
             data$filter_data[[i]]$covid_data$date[n_cases[i]])
    
    # Data is sorted from today to the past
    data$filter_data[[i]]$covid_data <-
      data$filter_data[[i]]$covid_data[
        -((n_cases[i] + 1):
            length(data$filter_data[[i]]$covid_data$cases)), ]
    
    len_max <- max(len_max, length(data$filter_data[[i]]$covid_data$cases))
  }
  
  # If figures are not plotted, figures are NULL
  fig1 <- fig2 <- fig3 <- fig4 <- fig5 <- fig6 <- NULL
  fig7 <- fig8 <- fig9 <- fig10 <- fig11 <- fig12 <- fig13 <- NULL
  if (plot_cases) {
    
    for (i in 1:length(data$filter_countries)) {
      if (i == 1) {
        
        fig1 <-
          plot_ly(data = data$filter_data[[i]]$covid_data,
                  x = ~date,
                  y = data$filter_data[[i]]$covid_data$cases /
                    rel_pop[i],
                  name = data$filter_countries[i],
                  colors = brewer.pal(n = length(data$filter_countries),
                                      name = "RdBu"),
                  type = 'scatter', mode = 'markers+lines',
                  marker = list(symbol = rand_markers[i], size = 8.5,
                                line = list(width = 2)))
        
      }
      else {
        
        fig1 <-
          fig1 %>% add_trace(data = data$filter_data[[i]]$covid_data,
                             x = ~date,
                             y = data$filter_data[[i]]$covid_data$cases /
                               rel_pop[i],
                             name = data$filter_countries[i],
                             type = 'scatter', mode = 'markers+lines',
                             colors =
                               brewer.pal(n = length(data$filter_countries),
                                          name = "RdBu"),
                             marker =
                               list(symbol = rand_markers[i], size = 8.5,
                                    line = list(width = 2)))
        
      }
    }
    
    # Layout details
    aux_title <- ifelse(log, "Daily log cases", "Daily cases")
    aux_title <- ifelse(is.logical(n_hab), aux_title,
                        paste(aux_title, "per each", n_hab, "hab."))
    fig1 <- fig1 %>%
      
      layout(title =
               ifelse(aligned_cases,
                      paste0(aux_title, " (aligned, day 0 = cum. cases > ",
                      100 * perc_pop, "% population)"),
                      ifelse(aligned_deaths,
                             paste0(aux_title, " (aligned, day 0 = cum. deaths > ",
                                    100 * perc_pop, "% population)"),
                             aux_title)),
             xaxis = list(range =
                            ifelse(aligned_cases | aligned_deaths,
                                   0:(len_max - 1),
                                   list(min(dates_allowed),
                                        format(Sys.time(), "%Y-%m-%d"))),
                          title =
                            ifelse(aligned_cases | aligned_deaths,
                                   "Days of pandemic", "dates"),
                                   zeroline = FALSE))
     

  }
  
  if(plot_deaths) {
    for (i in 1:length(data$filter_countries)) {
      
      if (i == 1) {
        
        fig2 <-
          plot_ly(data = data$filter_data[[i]]$covid_data,
                  x = ~date,
                  y = data$filter_data[[i]]$covid_data$deaths / rel_pop[i],
                  name = data$filter_countries[i],
                  colors = brewer.pal(n = length(data$filter_countries),
                                      name = "RdBu"),
                  type = 'scatter', mode = 'markers+lines',
                  marker = list(symbol = rand_markers[i], size = 8.5,
                                line = list(width = 2)))
        
      }
      else {
        
        fig2 <-
          fig2 %>% add_trace(data = data$filter_data[[i]]$covid_data,
                             x = ~date,
                             y = data$filter_data[[i]]$covid_data$deaths /
                               rel_pop[i],
                             name = data$filter_countries[i],
                             type = 'scatter', mode = 'markers+lines',
                             colors =
                               brewer.pal(n = length(data$filter_countries),
                                          name = "RdBu"),
                             marker =
                               list(symbol = rand_markers[i], size = 8.5,
                                    line = list(width = 2)))
        
      }
    }
    
    # Layout details
    aux_title <- ifelse(log, "Daily log deaths", "Daily deaths")
    aux_title <- ifelse(is.logical(n_hab), aux_title,
                        paste(aux_title, "per each", n_hab, "hab."))
    fig2 <- fig2 %>%
      
      layout(title =
               ifelse(aligned_cases,
                      paste0(aux_title, " (aligned, day 0 = cum. cases > ",
                             100 * perc_pop, "% population)"),
                      ifelse(aligned_deaths,
                             paste0(aux_title, " (aligned, day 0 = cum. deaths > ",
                                    100 * perc_pop, "% population)"),
                             aux_title)),
             xaxis = list(range =
                            ifelse(aligned_cases | aligned_deaths,
                                   0:(len_max - 1),
                                   list(min(dates_allowed),
                                        format(Sys.time(), "%Y-%m-%d"))),
                          title =
                            ifelse(aligned_cases | aligned_deaths,
                                   "Days of pandemic", "dates"),
                                   zeroline = FALSE))
 
   }

  if(plot_cum_cases) {

    for (i in 1:length(data$filter_countries)) {

      if (i == 1) {

        fig3 <-
          plot_ly(data = data$filter_data[[i]]$covid_data,
                  x = ~date,
                  y = data$filter_data[[i]]$covid_data$cum_cases / rel_pop[i],
                  name = data$filter_countries[i],
                  xaxis = list(range =
                                 list(min(dates_allowed),
                                      format(Sys.time(), "%Y-%m-%d"))),
                  colors = brewer.pal(n = length(data$filter_countries),
                                      name = "RdBu"),
                  type = 'scatter', mode = 'markers+lines',
                  marker = list(symbol = rand_markers[i], size = 8.5,
                                line = list(width = 2)))

      }
      else {

        fig3 <-
          fig3 %>% add_trace(data = data$filter_data[[i]]$covid_data,
                             x = ~date,
                             y = data$filter_data[[i]]$covid_data$cum_cases /
                               rel_pop[i],
                             name = data$filter_countries[i],
                             type = 'scatter', mode = 'markers+lines',
                             colors =
                               brewer.pal(n = length(data$filter_countries),
                                          name = "RdBu"),
                             marker =
                               list(symbol = rand_markers[i], size = 8.5,
                                    line = list(width = 2)))

      }
    }

    # Layout details
    aux_title <- paste("Cumulative cases", ifelse(is.logical(n_hab), "",
                        paste("per each", n_hab, "hab.")))
    fig3 <- fig3 %>%
      
      layout(title =
               ifelse(aligned_cases,
                      paste0(aux_title, " (aligned, day 0 = cum. cases > ",
                             100 * perc_pop, "% population)"),
                      ifelse(aligned_deaths,
                             paste0(aux_title, " (aligned, day 0 = cum. deaths > ",
                                    100 * perc_pop, "% population)"),
                             aux_title)),
             xaxis = list(range =
                            ifelse(aligned_cases | aligned_deaths,
                                   0:(len_max - 1),
                                   list(min(dates_allowed),
                                        format(Sys.time(), "%Y-%m-%d"))),
                          title =
                            ifelse(aligned_cases | aligned_deaths,
                                   "Days of pandemic", "dates"),
                                   zeroline = FALSE))

  }

  if(plot_cum_deaths) {

    for (i in 1:length(data$filter_countries)) {
      if (i == 1) {

        fig4 <-
          plot_ly(data = data$filter_data[[i]]$covid_data,
                  x = ~date,
                  y = data$filter_data[[i]]$covid_data$cum_deaths / rel_pop[i],
                  name = data$filter_countries[i],
                  colors = brewer.pal(n = length(ECDC_data$filter_countries),
                                      name = "RdBu"),
                  type = 'scatter', mode = 'markers+lines',
                  marker = list(symbol = rand_markers[i], size = 8.5,
                                line = list(width = 2)))

      }
      else {

        fig4 <-
          fig4 %>% add_trace(data = data$filter_data[[i]]$covid_data,
                             x = ~date,
                             y = data$filter_data[[i]]$covid_data$cum_deaths /
                               rel_pop[i],
                             name = data$filter_countries[i],
                             type = 'scatter', mode = 'markers+lines',
                             colors =
                               brewer.pal(n = length(data$filter_countries),
                                          name = "RdBu"),
                             marker =
                               list(symbol = rand_markers[i], size = 8.5,
                                    line = list(width = 2)))

      }
    }

    # Layout details
    aux_title <- paste("Cumulative deaths",
                       ifelse(is.logical(n_hab), "",
                              paste("per each", n_hab, "hab.")))
    fig4 <- fig4 %>%
      
      layout(title =
               ifelse(aligned_cases,
                      paste0(aux_title, " (aligned, day 0 = cum. cases > ",
                             100 * perc_pop, "% population)"),
                      ifelse(aligned_deaths,
                             paste0(aux_title, " (aligned, day 0 = cum. deaths > ",
                                    100 * perc_pop, "% population)"),
                             aux_title)),
             xaxis = list(range =
                            ifelse(aligned_cases | aligned_deaths,
                                   0:(len_max - 1),
                                   list(min(dates_allowed),
                                        format(Sys.time(), "%Y-%m-%d"))),
                          title =
                            ifelse(aligned_cases | aligned_deaths,
                                   "Days of pandemic", "dates"),
                                   zeroline = FALSE))

  }
  
  if(plot_v_cases) {


    for (i in 1:length(data$filter_countries)) {

      if (i == 1) {

        fig5 <-
          plot_ly(data = data$filter_data[[i]]$covid_data,
                  x = ~date,
                  y = data$filter_data[[i]]$covid_data$vel_cases,
                  name = data$filter_countries[i],
                  colors = brewer.pal(n = length(data$filter_countries),
                                      name = "RdBu"),
                  type = 'scatter', mode = 'markers+lines',
                  marker = list(symbol = rand_markers[i], size = 8.5,
                                line = list(width = 2)))

      }
      else {

        fig5 <-
          fig5 %>% add_trace(data = data$filter_data[[i]]$covid_data,
                             x = ~date,
                             y = data$filter_data[[i]]$covid_data$vel_cases,
                             name = data$filter_countries[i],
                             type = 'scatter', mode = 'markers+lines',
                             colors =
                               brewer.pal(n = length(data$filter_countries),
                                          name = "RdBu"),
                             marker =
                               list(symbol = rand_markers[i], size = 8.5,
                                    line = list(width = 2)))

      }
    }

    # Layout details
    aux_title <- "velocity of growth (% daily growth) of cases"
    fig5 <- fig5 %>% 
      layout(title =
               ifelse(aligned_cases,
                      paste0(aux_title, " (aligned, day 0 = cum. cases > ",
                             100 * perc_pop, "% population)"),
                      ifelse(aligned_deaths,
                             paste0(aux_title, " (aligned, day 0 = cum. deaths > ",
                                    100 * perc_pop, "% population)"),
                             aux_title)),
             xaxis = list(range =
                            ifelse(aligned_cases | aligned_deaths,
                                   0:(len_max - 1),
                                   list(min(dates_allowed),
                                        format(Sys.time(), "%Y-%m-%d"))),
                          title =
                            ifelse(aligned_cases | aligned_deaths,
                                   "Days of pandemic", "dates"),
                                   zeroline = FALSE))

  }

  if(plot_v_deaths) {

    for (i in 1:length(data$filter_countries)) {

      if (i == 1) {

        fig6 <-
          plot_ly(data = data$filter_data[[i]]$covid_data,
                  x = ~date,
                  y = data$filter_data[[i]]$covid_data$vel_deaths,
                  name = data$filter_countries[i],
                  colors = brewer.pal(n = length(data$filter_countries),
                                      name = "RdBu"),
                  type = 'scatter', mode = 'markers+lines',
                  marker = list(symbol = rand_markers[i], size = 8.5,
                                line = list(width = 2)))

      }
      else {

        fig6 <-
          fig6 %>% add_trace(data = data$filter_data[[i]]$covid_data,
                             x = ~date,
                             y = data$filter_data[[i]]$covid_data$vel_deaths,
                             name = data$filter_countries[i],
                             type = 'scatter', mode = 'markers+lines',
                             colors =
                               brewer.pal(n = length(ECDC_data$filter_countries),
                                          name = "RdBu"),
                             marker =
                               list(symbol = rand_markers[i], size = 8.5,
                                    line = list(width = 2)))

      }
    }

    # Layout details
    aux_title <- "velocity of growth (% daily growth) of deaths"
    fig6 <- fig6 %>%
      
      layout(title =
               ifelse(aligned_cases,
                      paste0(aux_title, " (aligned, day 0 = cum. cases > ",
                             100 * perc_pop, "% population)"),
                      ifelse(aligned_deaths,
                             paste0(aux_title, " (aligned, day 0 = cum. deaths > ",
                                    100 * perc_pop, "% population)"),
                             aux_title)),
             xaxis = list(range =
                            ifelse(aligned_cases | aligned_deaths,
                                   0:(len_max - 1),
                                   list(min(dates_allowed),
                                        format(Sys.time(), "%Y-%m-%d"))),
                          title =
                            ifelse(aligned_cases | aligned_deaths,
                                   "Days of pandemic", "dates"),
                                   zeroline = FALSE))

  }
  
  
  if(plot_a_cases) {
    
    
    for (i in 1:length(data$filter_countries)) {
      
      if (i == 1) {
        
        fig7 <-
          plot_ly(data = data$filter_data[[i]]$covid_data,
                  x = ~date,
                  y = data$filter_data[[i]]$covid_data$acc_cases,
                  name = data$filter_countries[i],
                  colors = brewer.pal(n = length(data$filter_countries),
                                      name = "RdBu"),
                  type = 'scatter', mode = 'markers+lines',
                  marker = list(symbol = rand_markers[i], size = 8.5,
                                line = list(width = 2)))
        
      }
      else {
        
        fig7 <-
          fig7 %>% add_trace(data = data$filter_data[[i]]$covid_data,
                             x = ~date,
                             y = data$filter_data[[i]]$covid_data$acc_cases,
                             name = data$filter_countries[i],
                             type = 'scatter', mode = 'markers+lines',
                             colors =
                               brewer.pal(n = length(data$filter_countries),
                                          name = "RdBu"),
                             marker =
                               list(symbol = rand_markers[i], size = 8.5,
                                    line = list(width = 2)))
        
      }
    }
    
    # Layout details
    aux_title <- "Acceleration (velocity of % daily growth) of cases"
    fig7 <- fig7 %>%
      
      layout(title =
               ifelse(aligned_cases,
                      paste0(aux_title, " (aligned, day 0 = cum. cases > ",
                             100 * perc_pop, "% population)"),
                      ifelse(aligned_deaths,
                             paste0(aux_title, " (aligned, day 0 = cum. deaths > ",
                                    100 * perc_pop, "% population)"),
                             aux_title)),
             xaxis = list(range =
                            ifelse(aligned_cases | aligned_deaths,
                                   0:(len_max - 1),
                                   list(min(dates_allowed),
                                        format(Sys.time(), "%Y-%m-%d"))),
                          title =
                            ifelse(aligned_cases | aligned_deaths,
                                   "Days of pandemic", "dates"),
                                   zeroline = FALSE))
    
  }
  
  if(plot_a_deaths) {
    
    for (i in 1:length(data$filter_countries)) {
      
      if (i == 1) {
        
        fig8 <-
          plot_ly(data = data$filter_data[[i]]$covid_data,
                  x = ~date,
                  y = data$filter_data[[i]]$covid_data$acc_deaths,
                  name = data$filter_countries[i],
                  colors = brewer.pal(n = length(data$filter_countries),
                                      name = "RdBu"),
                  type = 'scatter', mode = 'markers+lines',
                  marker = list(symbol = rand_markers[i], size = 8.5,
                                line = list(width = 2)))
        
      }
      else {
        
        fig8 <-
          fig8 %>% add_trace(data = data$filter_data[[i]]$covid_data,
                             x = ~date,
                             y = data$filter_data[[i]]$covid_data$acc_deaths,
                             name = data$filter_countries[i],
                             type = 'scatter', mode = 'markers+lines',
                             colors =
                               brewer.pal(n = length(ECDC_data$filter_countries),
                                          name = "RdBu"),
                             marker =
                               list(symbol = rand_markers[i], size = 8.5,
                                    line = list(width = 2)))
        
      }
    }
    
    # Layout details
    aux_title <- "Acceleration (velocity of % daily growth) of deaths"
    fig8 <- fig8 %>%
      
      layout(title =
               ifelse(aligned_cases,
                      paste0(aux_title, " (aligned, day 0 = cum. cases > ",
                             100 * perc_pop, "% population)"),
                      ifelse(aligned_deaths,
                             paste0(aux_title, " (aligned, day 0 = cum. deaths > ",
                                    100 * perc_pop, "% population)"),
                             aux_title)),
             xaxis = list(range =
                            ifelse(aligned_cases | aligned_deaths,
                                   0:(len_max - 1),
                                   list(min(dates_allowed),
                                        format(Sys.time(), "%Y-%m-%d"))),
                          title =
                            ifelse(aligned_cases | aligned_deaths,
                                   "Days of pandemic", "dates"),
                                   zeroline = FALSE))
    
  }
  
  if (plot_mort_rate) {

    for (i in 1:length(data$filter_countries)) {

      if (i == 1) {

        fig9 <-
          plot_ly(data = data$filter_data[[i]]$covid_data,
                  x = ~date,
                  y = data$filter_data[[i]]$covid_data$mort_rate,
                  name = data$filter_countries[i],
                  colors = brewer.pal(n = length(data$filter_countries),
                                      name = "RdBu"),
                  type = 'scatter', mode = 'markers+lines',
                  marker = list(symbol = rand_markers[i], size = 8.5,
                                line = list(width = 2)))

      }
      else {

        fig9 <-
          fig9 %>% add_trace(data = data$filter_data[[i]]$covid_data,
                             x = ~date,
                             y = data$filter_data[[i]]$covid_data$mort_rate,
                             name = data$filter_countries[i],
                             type = 'scatter', mode = 'markers+lines',
                             colors =
                               brewer.pal(n = length(data$filter_countries),
                                          name = "RdBu"),
                             marker =
                               list(symbol = rand_markers[i], size = 8.5,
                                    line = list(width = 2)))

      }
    }

    # Layout details
    aux_title <- "Mortality rates (cumulative deaths / cumulative cases)"
    fig9 <- fig9 %>%
      
      layout(title =
               ifelse(aligned_cases,
                      paste0(aux_title, " (aligned, day 0 = cum. cases > ",
                             100 * perc_pop, "% population)"),
                      ifelse(aligned_deaths,
                             paste0(aux_title, " (aligned, day 0 = cum. deaths > ",
                                    100 * perc_pop, "% population)"),
                             aux_title)),
             xaxis = list(range =
                            ifelse(aligned_cases | aligned_deaths,
                                   0:(len_max - 1),
                                   list(min(dates_allowed),
                                        format(Sys.time(), "%Y-%m-%d"))),
                          title =
                            ifelse(aligned_cases | aligned_deaths,
                                   "Days of pandemic", "dates"),
                                   zeroline = FALSE))
  }
    
  
  
  # Comparing with their continents
  if (plot_dev_by_continents) {
    
    # Cases
    for (i in 1:length(data$filter_countries)) {
      if (i == 1) {
        
        fig10 <-
          plot_ly(data = data$filter_data[[i]]$covid_data,
                  x = ~date,
                  y = data$filter_data[[i]]$covid_data$dev_cont_cases,
                  name = data$filter_countries[i],
                  colors = brewer.pal(n = length(data$filter_countries),
                                      name = "RdBu"),
                  type = 'scatter', mode = 'markers+lines',
                  marker = list(symbol = rand_markers[i], size = 8.5,
                                line = list(width = 2)))
        
        fig11 <-
          plot_ly(data = data$filter_data[[i]]$covid_data,
                  x = ~date,
                  y = data$filter_data[[i]]$covid_data$dev_cont_deaths,
                  name = data$filter_countries[i],
                  colors = brewer.pal(n = length(data$filter_countries),
                                      name = "RdBu"),
                  type = 'scatter', mode = 'markers+lines',
                  marker = list(symbol = rand_markers[i], size = 8.5,
                                line = list(width = 2)))
        
        fig12 <-
          plot_ly(data = data$filter_data[[i]]$covid_data,
                  x = ~date,
                  y = data$filter_data[[i]]$covid_data$dev_cont_cum_cases,
                  name = data$filter_countries[i],
                  colors = brewer.pal(n = length(data$filter_countries),
                                      name = "RdBu"),
                  type = 'scatter', mode = 'markers+lines',
                  marker = list(symbol = rand_markers[i], size = 8.5,
                                line = list(width = 2)))
        
        fig13 <-
          plot_ly(data = data$filter_data[[i]]$covid_data,
                  x = ~date,
                  y = data$filter_data[[i]]$covid_data$dev_cont_cum_deaths,
                  name = data$filter_countries[i],
                  colors = brewer.pal(n = length(data$filter_countries),
                                      name = "RdBu"),
                  type = 'scatter', mode = 'markers+lines',
                  marker = list(symbol = rand_markers[i], size = 8.5,
                                line = list(width = 2)))
        
      }
      else {
        
        fig10 <-
          fig10 %>% add_trace(data = data$filter_data[[i]]$covid_data,
                             x = ~date,
                             y = data$filter_data[[i]]$covid_data$dev_cont_cases,
                             name = data$filter_countries[i],
                             type = 'scatter', mode = 'markers+lines',
                             colors =
                               brewer.pal(n = length(data$filter_countries),
                                          name = "RdBu"),
                             marker =
                               list(symbol = rand_markers[i], size = 8.5,
                                    line = list(width = 2)))
        
        fig11 <-
          fig11 %>% add_trace(data = data$filter_data[[i]]$covid_data,
                              x = ~date,
                              y = data$filter_data[[i]]$covid_data$dev_cont_deaths,
                              name = data$filter_countries[i],
                              type = 'scatter', mode = 'markers+lines',
                              colors =
                                brewer.pal(n = length(data$filter_countries),
                                           name = "RdBu"),
                              marker =
                                list(symbol = rand_markers[i], size = 8.5,
                                     line = list(width = 2)))
        
        fig12 <-
          fig12 %>% add_trace(data = data$filter_data[[i]]$covid_data,
                              x = ~date,
                              y = data$filter_data[[i]]$covid_data$dev_cont_cum_cases,
                              name = data$filter_countries[i],
                              type = 'scatter', mode = 'markers+lines',
                              colors =
                                brewer.pal(n = length(data$filter_countries),
                                           name = "RdBu"),
                              marker =
                                list(symbol = rand_markers[i], size = 8.5,
                                     line = list(width = 2)))
        
        fig13 <-
          fig13 %>% add_trace(data = data$filter_data[[i]]$covid_data,
                              x = ~date,
                              y = data$filter_data[[i]]$covid_data$dev_cont_cum_deaths,
                              name = data$filter_countries[i],
                              type = 'scatter', mode = 'markers+lines',
                              colors =
                                brewer.pal(n = length(data$filter_countries),
                                           name = "RdBu"),
                              marker =
                                list(symbol = rand_markers[i], size = 8.5,
                                     line = list(width = 2)))
        
      }
    }

    # fig10: layout details
    aux_title <- paste("Daily dev. (resp. to their continentes) of",
                       ifelse(log, "log cases", "cases"))
    fig10 <- fig10 %>%
      layout(title =
               ifelse(aligned_cases,
                      paste0(aux_title, " (aligned, day 0 = cum. cases > ",
                             100 * perc_pop, "% population)"),
                      ifelse(aligned_deaths,
                             paste0(aux_title, " (aligned, day 0 = cum. deaths > ",
                                    100 * perc_pop, "% population)"),
                             aux_title)),
             xaxis = list(range =
                            ifelse(aligned_cases | aligned_deaths,
                                   0:(len_max - 1),
                                   list(min(dates_allowed),
                                        format(Sys.time(), "%Y-%m-%d"))),
                          title =
                            ifelse(aligned_cases | aligned_deaths,
                                   "Days of pandemic", "dates"),
                          zeroline = FALSE))
    
    # fig11: layout details
    aux_title <- paste("Daily dev. (resp. to their continentes) of",
                       ifelse(log, "log cases", "deaths"))
    fig11 <- fig11 %>%
      layout(title =
               ifelse(aligned_cases,
                      paste0(aux_title, " (aligned, day 0 = cum. cases > ",
                             100 * perc_pop, "% population)"),
                      ifelse(aligned_deaths,
                             paste0(aux_title, " (aligned, day 0 = cum. deaths > ",
                                    100 * perc_pop, "% population)"),
                             aux_title)),
             xaxis = list(range =
                            ifelse(aligned_cases | aligned_deaths,
                                   0:(len_max - 1),
                                   list(min(dates_allowed),
                                        format(Sys.time(), "%Y-%m-%d"))),
                          title =
                            ifelse(aligned_cases | aligned_deaths,
                                   "Days of pandemic", "dates"),
                          zeroline = FALSE))
    
    # fig12: layout details
    aux_title <- paste("Daily dev. (resp. to their continentes) of",
                       ifelse(log, "log cases", "cum. cases"))
    fig12 <- fig12 %>%
      layout(title =
               ifelse(aligned_cases,
                      paste0(aux_title, " (aligned, day 0 = cum. cases > ",
                             100 * perc_pop, "% population)"),
                      ifelse(aligned_deaths,
                             paste0(aux_title, " (aligned, day 0 = cum. deaths > ",
                                    100 * perc_pop, "% population)"),
                             aux_title)),
             xaxis = list(range =
                            ifelse(aligned_cases | aligned_deaths,
                                   0:(len_max - 1),
                                   list(min(dates_allowed),
                                        format(Sys.time(), "%Y-%m-%d"))),
                          title =
                            ifelse(aligned_cases | aligned_deaths,
                                   "Days of pandemic", "dates"),
                          zeroline = FALSE))
    
    # fig13: layout details
    aux_title <- paste("Daily dev. (resp. to their continentes) of",
                       ifelse(log, "log cases", "cum. deaths"))
    fig13 <- fig13 %>%
      layout(title =
               ifelse(aligned_cases,
                      paste0(aux_title, " (aligned, day 0 = cum. cases > ",
                             100 * perc_pop, "% population)"),
                      ifelse(aligned_deaths,
                             paste0(aux_title, " (aligned, day 0 = cum. deaths > ",
                                    100 * perc_pop, "% population)"),
                             aux_title)),
             xaxis = list(range =
                            ifelse(aligned_cases | aligned_deaths,
                                   0:(len_max - 1),
                                   list(min(dates_allowed),
                                        format(Sys.time(), "%Y-%m-%d"))),
                          title =
                            ifelse(aligned_cases | aligned_deaths,
                                   "Days of pandemic", "dates"),
                          zeroline = FALSE))

  }
  
  # Output figures
  return(list("fig_cases" = fig1, "fig_deaths" = fig2,
              "fig_cum_cases" = fig3, "fig_cum_deaths" = fig4,
              "fig_vel_cases" = fig5, "fig_vel_deaths" = fig6,
              "fig_acc_cases" = fig7, "fig_acc_deaths" = fig8,
              "fig_mort_rate" = fig9, "fig_dev_cont_cases" = fig10,
              "fig_dev_cont_deaths" = fig11, "fig_dev_cont_cum_cases" = fig12,
              "fig_dev_cont_cum_deaths" = fig13))
}








plot_vel_growth <- function(ECDC_country, rate = 2, from = 7,
                            each_days = c(2, 3, 4, 5)) {
  
  fig <- plot_ly(x = ECDC_country$covid_data$date[1:from],
                 y = rev((rate^((0:(from - 1))/each_days[1])) *
                           ECDC_country$covid_data$cum_inf[from]),
                 name = paste0("x", rate, " each ",
                               each_days[1], " days"),
                 colors = brewer.pal(n = 1 + length(each_days), name = "RdBu"),
                 type = 'scatter', mode = 'markers+lines',
                 marker = list(symbol = "circle-open", size = 7.5,
                               line = list(width = 1.5)),
                 fill = 'tozeroy')
  
  len <- length(ECDC_country$covid_data$date)
  for (i in 2:length(each_days)) {
    
    fig <- fig %>% add_lines(x = ECDC_country$covid_data$date[1:from],
                             y = rev((rate^((0:(from - 1))/each_days[i])) *
                               ECDC_country$covid_data$cum_inf[from]),
                             name = paste0("x", rate, " each ",
                                           each_days[i], " days"),
                             line = list(width = 2, dash = 'dash'),
                             fill = 'tozeroy')
  
  }
  fig <- fig %>% add_lines(data = ECDC_country$covid_data,
                           x = ~date, y = ~cum_inf,
                           name = ECDC_country$country,
                           line = list(width = 2, dash = 'dash'),
                           fill = 'tozeroy')
    
  # Layout details
  fig <- fig %>%
      layout(title = paste('Comparing the growth of infected of',
                           ECDC_country$country),
             yaxis = list(title = 'cumulative infected', zeroline = FALSE),
             xaxis = list(title = 'dates', zeroline = FALSE))
  
  # Output figures
  return(list("fig_vel_growth" = fig))

}

