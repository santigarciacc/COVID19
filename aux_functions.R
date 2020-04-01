library(utils)
library(httr)

filtering_data_covid19 <-
  function(raw_data, max_date = format(Sys.time(), "%Y-%m-%d"),
           dates = as.Date(c("2019-12-31", max_date)),
           countries = c("ESP", "ITA", "DEU", "FRA", "CHN", "KOR", "USA",
                         "GBR", "IRN", "MEX")) {
    
    # Check if all countries are allowed
    if (!all(countries %in% raw_data$countryterritoryCode)) {
      
      stop(paste("Sorry, some of the country codes is wrong",
                 "or the country is not currently available"))
      
    }
    
    # Force dates are between "2019-12-31" (1st row) and max_date (<= today)
    dates <- pmax(as.Date("2019-12-31"),
                  pmin(dates, as.Date(format(Sys.time(), "%Y-%m-%d"))))
    
    # Filter the data according to dates and countries selected
    filtered_data <-
      raw_data[(raw_data$countryterritoryCode %in% countries) &
                  (as.Date(raw_data$dateRep, "%d-%m-%Y", tz = "UTC") %in%
                                            seq(dates[1], dates[2], by = 1)), ]

    # Provide the data.frame "ECDC_" + country code for each country
    output_list <- list()
    for(c in 1:length(countries)) {
      
      na_rows <- sum(is.na(raw_data$countryterritoryCode == countries[c]))
      country_index <-
        which(na.omit(raw_data$countryterritoryCode == countries[c])) + na_rows
      filtered_date <- as.Date(raw_data$dateRep[country_index], "%d-%m-%Y",
                               tz = "UTC")
      
      aux_cumulative <- cumsum(rev(raw_data$cases[country_index]))[-1] /
        rev(rev(cumsum(rev(raw_data$cases[country_index])))[-1])
      
      aux_cumulative_deads <- cumsum(rev(raw_data$deaths[country_index]))[-1] /
        rev(rev(cumsum(rev(raw_data$deaths[country_index])))[-1])
      
      output_list[[c]] <-
        assign(paste0("ECDC_", countries[c]),
               list("covid_data" =
                      data.frame("date" = filtered_date,
                                 "cases" = raw_data$cases[country_index],
                                 "cum_cases" =
                                   rev(cumsum(rev(raw_data$cases[country_index]))),
                                 "deaths" = raw_data$deaths[country_index],
                                 "cum_deaths" =
                                   rev(cumsum(rev(raw_data$deaths[country_index]))),
                                 "vel_cases" =
                                   rev(c(0, ifelse(is.nan(aux_cumulative), 0,
                                                   aux_cumulative - 1))),
                                 "vel_deaths" =
                                   rev(c(0, ifelse(is.nan(aux_cumulative_deads), 0,
                                                   aux_cumulative_deads - 1))),
                                 "mort_rate" =
                                   rev(cumsum(rev(raw_data$deaths[country_index]))) /
                                   rev(cumsum(rev(raw_data$cases[country_index])))),
                                 "country" = countries[c],
                                 "population" =
                      raw_data$popData2018[country_index[1]]))

      # Providing the velocity of the growth (acceleration) of cases
      aux_acc_cases <- rev(rev(output_list[[c]]$covid_data$vel_cases)[-1]) /
        output_list[[c]]$covid_data$vel_cases[-1]
      output_list[[c]]$covid_data$acc_cases <-
        c(ifelse(is.nan(aux_acc_cases), 0, aux_acc_cases - 1), 0)
      
      # Removing also Inf values (fixing as 0)
      output_list[[c]]$covid_data$acc_cases <-
        ifelse(is.infinite(output_list[[c]]$covid_data$acc_cases),
               0, output_list[[c]]$covid_data$acc_cases)
               
      # Providing the velocity of the growth (acceleration) of deaths
      aux_acc_deaths <- rev(rev(output_list[[c]]$covid_data$vel_deaths)[-1]) /
        output_list[[c]]$covid_data$vel_deaths[-1]
      output_list[[c]]$covid_data$acc_deaths <-
        c(ifelse(is.nan(aux_acc_deaths), 0, aux_acc_deaths - 1), 0)
      
      # Removing also Inf values (fixing as 0)
      output_list[[c]]$covid_data$acc_deaths <-
        ifelse(is.infinite(output_list[[c]]$covid_data$acc_deaths),
               0, output_list[[c]]$covid_data$acc_deaths)
    }
    
    # Output
    return(output_list)
    
  }


extracting_ECDC_covid19 <-
  function(source = paste0("https://www.ecdc.europa.eu/sites/",
                           "default/files/documents/",
                           "COVID-19-geographic-disbtribution-worldwide.xlsx"),
           file_format = ".xlsx",
           http_auth = authenticate(":", ":", type = "ntlm"),
           dates = as.Date(c("2019-12-31", format(Sys.time(), "%Y-%m-%d"))),
           countries = c("ESP", "ITA", "DEU", "FRA", "CHN", "KOR", "USA",
                         "GBR", "IRN"), save_local = FALSE) {
  
    # Downloading the dataset from the url to our local as a temporary file
    GET(source, http_auth, write_disk(temp_file <-
                                     tempfile(fileext = file_format)))

    # After downloading, we convert the dataset into a R list
    raw_data <- read_xlsx(temp_file)

    # We extract a list of the countries and territories included in the dataset
    all_lands <- unique(raw_data$countryterritoryCode)
    
    # Just the data of the countries and dates provided
    filter_data <- filtering_data_covid19(raw_data, dates = dates,
                                          countries = countries)
    
    if (save_local) {
      
      save(raw_data, file = paste0("raw_ECDC_data_",
                                   format(Sys.time(), "%d-%m-%Y"), ".RData"))
      save(filter_data, file = paste0("filtered_ECDC_data_",
                                      format(Sys.time(), "%d-%m-%Y"), ".RData"))
      
    }
    
    # Output
    return(list("raw_data" = raw_data, "filter_data" = filter_data,
                "all_countries" = all_lands,
                "filter_countries" = countries))
    
}


##
## PLOTS
##

comparative_countries <- function(data, log_10 = FALSE, n_hab = 1,
                                  aligned_cases = FALSE, aligned_deaths = FALSE,
                                  plot_cases = TRUE, plot_deaths = TRUE,
                                  plot_cum_cases = TRUE, plot_cum_deaths = TRUE,
                                  plot_mort_rate = TRUE,
                                  vel_mult = FALSE, plot_v_cases = TRUE,
                                  plot_v_deaths = TRUE, plot_a_cases = TRUE,
                                  plot_a_deaths = TRUE,
                                  rate = NULL, each_days = NULL, from = NULL) {
  
  # Number of countries to be compared and number of variables (column)
  n_countries <- length(data$filter_countries)
  
  # If log_10 == TRUE, we convert all data to logarithmic scale
  rel_pop <- rep(0, n_countries)
  for (i in 1:n_countries) {
 
    if (log_10) {
      
      data$filter_data[[i]]$covid_data[ , 2:5] <-
        lapply(log(data$filter_data[[i]]$covid_data[, 2:5]),
               FUN = function(x) replace(x, is.infinite(x), 0))
    }

    if (aligned_cases) {
      
      data$filter_data[[i]]$covid_data <-
        data$filter_data[[i]]$covid_data[
          data$filter_data[[i]]$covid_data$cum_cases > 100, ]
      
      data$filter_data[[i]]$covid_data$date <-
        rev(0:(length(data$filter_data[[i]]$covid_data$date) - 1))
    }
      
    if (aligned_deaths) {
      
      data$filter_data[[i]]$covid_data <-
        data$filter_data[[i]]$covid_data[
          data$filter_data[[i]]$covid_data$cum_deaths > 10, ]
      
      data$filter_data[[i]]$covid_data$date <-
        rev(0:(length(data$filter_data[[i]]$covid_data$date) - 1))
    }
    
    # Relative population
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
  
  # Each country is stored in a list named as "ECDC_" + country code
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
             sum(data$filter_data[[i]]$covid_data$cum_cases > 100),
                         ifelse(aligned_deaths,
                                sum(data$filter_data[[i]]$covid_data$cum_cases > 10),
                                length(data$filter_data[[i]]$covid_data$cum_cases)))
    
    dates_allowed[i] <-
      ifelse(aligned_cases | aligned_deaths, 0,
             data$filter_data[[i]]$covid_data$date[n_cases[i]])
    
    data$filter_data[[i]]$covid_data <-
      data$filter_data[[i]]$covid_data[
        -((n_cases[i] + 1):
            length(data$filter_data[[i]]$covid_data$cum_cases)), ]
    
    assign(paste0("ECDC_", data$filter_countries[i]),
           data$filter_data[[i]])
    
    len_max <- max(len_max, length(data$filter_data[[i]]$covid_data$cum_cases))
  }
  
  fig1 <- fig2 <- fig3 <- fig4 <- fig5 <- fig6 <- fig7 <- fig8 <- fig9 <- NULL
  if (plot_cases) {
    
    for (i in 1:length(data$filter_countries)) {
      if (i == 1) {
        
        fig1 <-
          plot_ly(data =
                    get(paste0("ECDC_",
                               data$filter_countries[i]))$covid_data,
                  x = ~date,
                  y = data$filter_data[[i]]$covid_data$cases / rel_pop[i],
                  name = data$filter_countries[i],
                  colors = brewer.pal(n = length(data$filter_countries),
                                      name = "RdBu"),
                  type = 'scatter', mode = 'markers+lines',
                  marker = list(symbol = rand_markers[i], size = 8.5,
                                line = list(width = 2)))
        
      }
      else {
        
        fig1 <-
          fig1 %>% add_trace(data = get(paste0("ECDC_",
                                               data$filter_countries[i]))$covid_data,
                             x = ~date,
                             y = data$filter_data[[i]]$covid_data$cases / rel_pop[i],
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
    aux_title <- ifelse(log_10, "Daily log10 cases", "Daily cases")
    aux_title <- ifelse(is.logical(n_hab), aux_title,
                        paste(aux_title, "per each", n_hab, "people"))
    fig1 <- fig1 %>%
      layout(title =
               ifelse(aligned_cases,
                      paste(aux_title, "(aligned, day 0 = cumul. cases > 100)"),
                      ifelse(aligned_deaths,
                      paste(aux_title, "(aligned, day 0 = cumul. deaths > 10)"),
                      aux_title)),
             yaxis = list(title = paste("new", aux_title), zeroline = FALSE),
             xaxis = list(range =
                            ifelse(aligned_cases | aligned_deaths,
                                   0:(len_max - 1),
                                   list(min(dates_allowed),
                                        format(Sys.time(), "%Y-%m-%d"))),
                          title = 'dates', zeroline = FALSE))

  }
  
  if(plot_deaths) {
    for (i in 1:length(data$filter_countries)) {
      
      if (i == 1) {
        
        fig2 <-
          plot_ly(data =
                    get(paste0("ECDC_",
                               data$filter_countries[i]))$covid_data,
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
          fig2 %>% add_trace(data = get(paste0("ECDC_",
                                               data$filter_countries[i]))$covid_data,
                             x = ~date,
                             y = data$filter_data[[i]]$covid_data$deaths / rel_pop[i],
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
    aux_title <- ifelse(log_10, "Daily log10 deaths", "Daily deaths")
    aux_title <- ifelse(is.logical(n_hab), aux_title,
                        paste(aux_title, "per each", n_hab, "people"))
    fig2 <- fig2 %>%
      layout(title =
               ifelse(aligned_cases,
                      paste(aux_title, "(aligned, day 0 = cumul. cases > 100)"),
                      ifelse(aligned_deaths,
                             paste(aux_title, "(aligned, day 0 = cumul. deaths > 10)"),
                             aux_title)),
             yaxis = list(title = paste("new", aux_title), zeroline = FALSE),
             xaxis = list(range =
                            ifelse(aligned_cases | aligned_deaths,
                                   0:(len_max - 1),
                                   list(min(dates_allowed),
                                        format(Sys.time(), "%Y-%m-%d"))),
                          title = 'dates', zeroline = FALSE))
 
   }

  if(plot_cum_cases) {

    for (i in 1:length(data$filter_countries)) {

      if (i == 1) {

        fig3 <-
          plot_ly(data =
                    get(paste0("ECDC_",
                               data$filter_countries[i]))$covid_data,
                  x = ~date,
                  y = data$filter_data[[i]]$covid_data$cum_cases / rel_pop[i],
                  xaxis = list(range =
                                 list(min(dates_allowed),
                                      format(Sys.time(), "%Y-%m-%d"))),
                  name = data$filter_countries[i],
                  colors = brewer.pal(n = length(data$filter_countries),
                                      name = "RdBu"),
                  type = 'scatter', mode = 'markers+lines',
                  marker = list(symbol = rand_markers[i], size = 8.5,
                                line = list(width = 2)))

      }
      else {

        fig3 <-
          fig3 %>% add_trace(data = get(paste0("ECDC_",
                                               data$filter_countries[i]))$covid_data,
                             x = ~date,
                             y = data$filter_data[[i]]$covid_data$cum_cases / rel_pop[i],
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
                        paste("per each", n_hab, "people")))
    fig3 <- fig3 %>%
      layout(title =
               ifelse(aligned_cases,
                      paste(aux_title, "(aligned, day 0 = cumul. cases > 100)"),
                      ifelse(aligned_deaths,
                             paste(aux_title, "(aligned, day 0 = cumul. deaths > 10)"),
                             aux_title)),
             yaxis = list(title = paste("new", aux_title), zeroline = FALSE),
             xaxis = list(range =
                            ifelse(aligned_cases | aligned_deaths,
                                   0:(len_max - 1),
                                   list(min(dates_allowed),
                                        format(Sys.time(), "%Y-%m-%d"))),
                          title = 'dates', zeroline = FALSE))

  }

  if(plot_cum_deaths) {

    for (i in 1:length(data$filter_countries)) {
      if (i == 1) {

        fig4 <-
          plot_ly(data =
                    get(paste0("ECDC_",
                               data$filter_countries[i]))$covid_data,
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
          fig4 %>% add_trace(data = get(paste0("ECDC_",
                                               data$filter_countries[i]))$covid_data,
                             x = ~date, 
                             y = data$filter_data[[i]]$covid_data$cum_deaths / rel_pop[i],
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
                              paste("per each", n_hab, "people")))
    fig4 <- fig4 %>%
      layout(title =
               ifelse(aligned_cases,
                      paste(aux_title, "(aligned, day 0 = cumul. cases > 100)"),
                      ifelse(aligned_deaths,
                             paste(aux_title, "(aligned, day 0 = cumul. deaths > 10)"),
                             aux_title)),
             yaxis = list(title = paste("new", aux_title), zeroline = FALSE),
             xaxis = list(range =
                            ifelse(aligned_cases | aligned_deaths,
                                   0:(len_max - 1),
                                   list(min(dates_allowed),
                                        format(Sys.time(), "%Y-%m-%d"))),
                          title = 'dates', zeroline = FALSE))

  }
  
  if(plot_v_cases) {


    for (i in 1:length(data$filter_countries)) {

      if (i == 1) {

        fig5 <-
          plot_ly(data =
                    get(paste0("ECDC_",
                               data$filter_countries[i]))$covid_data,
                  x = ~date, y = ~vel_cases,
                  name = data$filter_countries[i],
                  colors = brewer.pal(n = length(data$filter_countries),
                                      name = "RdBu"),
                  type = 'scatter', mode = 'markers+lines',
                  marker = list(symbol = rand_markers[i], size = 8.5,
                                line = list(width = 2)))

      }
      else {

        fig5 <-
          fig5 %>% add_trace(data = get(paste0("ECDC_",
                                               data$filter_countries[i]))$covid_data,
                             x = ~date, y = ~vel_cases,
                             name = ECDC_data$filter_countries[i],
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
                      paste(aux_title, "(aligned, day 0 = cumul. cases > 100)"),
                      ifelse(aligned_deaths,
                             paste(aux_title, "(aligned, day 0 = cumul. deaths > 10)"),
                             aux_title)),
             yaxis = list(title = paste("new", aux_title), zeroline = FALSE),
             xaxis = list(range =
                            ifelse(aligned_cases | aligned_deaths,
                                   0:(len_max - 1),
                                   list(min(dates_allowed),
                                        format(Sys.time(), "%Y-%m-%d"))),
                          title = 'dates', zeroline = FALSE))

  }

  if(plot_v_deaths) {

    for (i in 1:length(data$filter_countries)) {

      if (i == 1) {

        fig6 <-
          plot_ly(data =
                    get(paste0("ECDC_",
                               data$filter_countries[i]))$covid_data,
                  x = ~date, y = ~vel_deaths,
                  name = data$filter_countries[i],
                  colors = brewer.pal(n = length(data$filter_countries),
                                      name = "RdBu"),
                  type = 'scatter', mode = 'markers+lines',
                  marker = list(symbol = rand_markers[i], size = 8.5,
                                line = list(width = 2)))

      }
      else {

        fig6 <-
          fig6 %>% add_trace(data = get(paste0("ECDC_",
                                               data$filter_countries[i]))$covid_data,
                             x = ~date, y = ~vel_deaths,
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
                      paste(aux_title, "(aligned, day 0 = cumul. cases > 100)"),
                      ifelse(aligned_deaths,
                             paste(aux_title, "(aligned, day 0 = cumul. deaths > 10)"),
                             aux_title)),
             yaxis = list(title = paste("new", aux_title), zeroline = FALSE),
             xaxis = list(range =
                            ifelse(aligned_cases | aligned_deaths,
                                   0:(len_max - 1),
                                   list(min(dates_allowed),
                                        format(Sys.time(), "%Y-%m-%d"))),
                          title = 'dates', zeroline = FALSE))

  }
  
  
  if(plot_a_cases) {
    
    
    for (i in 1:length(data$filter_countries)) {
      
      if (i == 1) {
        
        fig7 <-
          plot_ly(data =
                    get(paste0("ECDC_",
                               data$filter_countries[i]))$covid_data,
                  x = ~date, y = ~acc_cases,
                  name = data$filter_countries[i],
                  colors = brewer.pal(n = length(data$filter_countries),
                                      name = "RdBu"),
                  type = 'scatter', mode = 'markers+lines',
                  marker = list(symbol = rand_markers[i], size = 8.5,
                                line = list(width = 2)))
        
      }
      else {
        
        fig7 <-
          fig7 %>% add_trace(data = get(paste0("ECDC_",
                                               data$filter_countries[i]))$covid_data,
                             x = ~date, y = ~acc_cases,
                             name = ECDC_data$filter_countries[i],
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
                      paste(aux_title, "(aligned, day 0 = cumul. cases > 100)"),
                      ifelse(aligned_deaths,
                             paste(aux_title, "(aligned, day 0 = cumul. deaths > 10)"),
                             aux_title)),
             yaxis = list(title = paste("new", aux_title), zeroline = FALSE),
             xaxis = list(range =
                            ifelse(aligned_cases | aligned_deaths,
                                   0:(len_max - 1),
                                   list(min(dates_allowed),
                                        format(Sys.time(), "%Y-%m-%d"))),
                          title = 'dates', zeroline = FALSE))
    
  }
  
  if(plot_a_deaths) {
    
    for (i in 1:length(data$filter_countries)) {
      
      if (i == 1) {
        
        fig8 <-
          plot_ly(data =
                    get(paste0("ECDC_",
                               data$filter_countries[i]))$covid_data,
                  x = ~date, y = ~acc_deaths,
                  name = data$filter_countries[i],
                  colors = brewer.pal(n = length(data$filter_countries),
                                      name = "RdBu"),
                  type = 'scatter', mode = 'markers+lines',
                  marker = list(symbol = rand_markers[i], size = 8.5,
                                line = list(width = 2)))
        
      }
      else {
        
        fig8 <-
          fig8 %>% add_trace(data = get(paste0("ECDC_",
                                               data$filter_countries[i]))$covid_data,
                             x = ~date, y = ~acc_deaths,
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
                      paste(aux_title, "(aligned, day 0 = cumul. cases > 100)"),
                      ifelse(aligned_deaths,
                             paste(aux_title, "(aligned, day 0 = cumul. deaths > 10)"),
                             aux_title)),
             yaxis = list(title = paste("new", aux_title), zeroline = FALSE),
             xaxis = list(range =
                            ifelse(aligned_cases | aligned_deaths,
                                   0:(len_max - 1),
                                   list(min(dates_allowed),
                                        format(Sys.time(), "%Y-%m-%d"))),
                          title = 'dates', zeroline = FALSE))
    
  }
  
  if (plot_mort_rate) {

    for (i in 1:length(data$filter_countries)) {

      if (i == 1) {

        fig9 <-
          plot_ly(data =
                    get(paste0("ECDC_",
                               data$filter_countries[i]))$covid_data,
                  x = ~date, y = ~mort_rate,
                  name = data$filter_countries[i],
                  colors = brewer.pal(n = length(data$filter_countries),
                                      name = "RdBu"),
                  type = 'scatter', mode = 'markers+lines',
                  marker = list(symbol = rand_markers[i], size = 8.5,
                                line = list(width = 2)))

      }
      else {

        fig9 <-
          fig9 %>% add_trace(data = get(paste0("ECDC_",
                                               data$filter_countries[i]))$covid_data,
                             x = ~date, y = ~mort_rate,
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
                      paste(aux_title, "(aligned, day 0 = cumul. cases > 100)"),
                      ifelse(aligned_deaths,
                             paste(aux_title, "(aligned, day 0 = cumul. deaths > 10)"),
                             aux_title)),
             yaxis = list(title = paste("new", aux_title), zeroline = FALSE),
             xaxis = list(range =
                            ifelse(aligned_cases | aligned_deaths,
                                   0:(len_max - 1),
                                   list(min(dates_allowed),
                                        format(Sys.time(), "%Y-%m-%d"))),
                          title = 'dates', zeroline = FALSE))
  }
  
  # Output figures
  return(list("fig_cases" = fig1, "fig_deaths" = fig2,
              "fig_cum_cases" = fig3, "fig_cum_deaths" = fig4,
              "fig_vel_cases" = fig5, "fig_vel_deaths" = fig6,
              "fig_acc_cases" = fig7, "fig_acc_deaths" = fig8,
              "fig_mort_rate" = fig9))
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

