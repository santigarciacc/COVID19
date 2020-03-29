library(utils)
library(httr)

filtering_ECDC_data_covid19 <-
  function(ECDC_data, aligned = FALSE, min_cases = 100,
           max_date = format(Sys.time(), "%Y-%m-%d"),
           dates = as.Date(c("2019-12-31", max_date)),
           countries = c("ESP", "ITA", "DEU", "FRA", "CHN", "KOR", "USA",
                         "GBR", "IRN")) {
    
    # Check if all countries are allowed
    if (any(countries %in% ECDC_data$countries)) {
      
      stop("Some of the country codes is wrong")
      
    }
    
    # Force dates are between "2019-12-31" (1st row) and max_date (<= today)
    dates <- pmax(as.Date("2019-12-31"),
                  pmin(dates, as.Date(format(Sys.time(), "%Y-%m-%d"))))
    
    # Filter the data according to dates and countries selected
    filtered_data <-
      ECDC_data[(ECDC_data$countryterritoryCode %in% countries) &
                  (as.Date(as.character(ECDC_data$dateRep), "%d/%m/%Y") %in%
                                            seq(dates[1], dates[2], by = 1)), ]
    
    # Provide the data.frame "ECDC_" + country code for each country
    output_list <- list()
    for(c in 1:length(countries)) {
      
      country_index <- ECDC_data$countryterritoryCode == countries[c]
      filtered_date <- as.Date(as.character(ECDC_data$dateRep[country_index]),
                               "%d/%m/%Y")
      
      aux_cumulative <- cumsum(rev(ECDC_data$cases[country_index]))[-1] /
        rev(rev(cumsum(rev(ECDC_data$cases[country_index])))[-1])
      
      aux_cumulative_deaths <- cumsum(rev(ECDC_data$deaths[country_index]))[-1] /
        rev(rev(cumsum(rev(ECDC_data$deaths[country_index])))[-1])
      
      output_list[[c]] <-
        assign(paste0("ECDC_", countries[c]),
               list("covid_data" =
                      data.frame("date" = filtered_date,
                                 "inc_inf" = ECDC_data$cases[country_index],
                                 "cum_inf" =
                                   rev(cumsum(rev(ECDC_data$cases[country_index]))),
                                 "growth_inf" =
                                   rev(c(0, ifelse(is.nan(aux_cumulative), 0,
                                                   aux_cumulative - 1))),
                                 "inc_deaths" = ECDC_data$deaths[country_index],
                                 "cum_deaths" =
                                   rev(cumsum(rev(ECDC_data$deaths[country_index]))),
                                 "growth_deaths" =
                                   rev(c(0, ifelse(is.nan(aux_cumulative_deaths), 0,
                                                   aux_cumulative_deaths - 1))),
                                 "mortality_rate" =
                                   rev(cumsum(rev(ECDC_data$deaths[country_index]))) /
                                   rev(cumsum(rev(ECDC_data$cases[country_index])))),
                                 "country" = countries[c],
                                 "population" =
                      ECDC_data$popData2018[which(country_index)[1]]))
      
      if (aligned) {
        
        output_list[[c]]$covid_data <-
          output_list[[c]]$covid_data[
            output_list[[c]]$covid_data$cum_inf > min_cases, ]
        
        output_list[[c]]$covid_data$date <-
          rev(0:(length(output_list[[c]]$covid_data$date) - 1))

      }
    }
    
    # Output
    return(output_list)
    
  }


extracting_data_covid19 <-
  function(source = paste0("https://opendata.ecdc.europa.eu/covid19/",
                           "casedistribution/csv"),
           file_format = ".csv",
           http_auth = authenticate(":", ":", type = "ntlm"),
           dates = as.Date(c("2019-12-31", format(Sys.time(), "%Y-%m-%d"))),
           countries = c("ESP", "ITA", "DEU", "FRA", "CHN", "KOR", "USA",
                         "GBR", "IRN"), save_local = FALSE, aligned = FALSE,
           min_cases = 100) {
    
    # Downloading the dataset from the url to our local as a temporary file
    GET(source, http_auth, write_disk(temp_file <-
                                     tempfile(fileext = file_format)))
    
    # After downloading, we convert the dataset into a R list
    raw_data <- read.csv(temp_file)

    # We extract a list of the countries and territories included in the dataset
    all_lands <- unique(raw_data$countryterritoryCode)
    
    # Just the data of the countries and dates provided
    filter_data <- filtering_ECDC_data_covid19(raw_data, dates = dates,
                                               countries = countries,
                                               aligned = aligned,
                                               min_cases = min_cases)
    
    if (save_local) {
      
      save(raw_data, file = paste0("raw_ECDC_data",
                                   format(Sys.time(), "%d-%m-%Y"), ".RData"))
      save(filter_data, file = paste0("filtered_ECDC_data",
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

comparative_countries <- function(ECDC_data, threshold = FALSE, aligned = FALSE,
                                 min_cases = 100, inc_inf = TRUE,
                                 inc_deaths = FALSE, cum_inf = TRUE,
                                 cum_deaths = FALSE, growth_inf = TRUE,
                                 growth_deaths = FALSE, 
                                 mortality_rate = FALSE) {
  
  threshold <- ifelse(aligned, FALSE, threshold)

  # Forcing that min_cases should be >= 0
  min_cases <- max(min_cases, 0)
  
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
  max_countries <- length(plot_markers)
  if (length(plot_markers) < length(ECDC_data$filter_countries)) {
    
    ECDC_data$filter_countries[(max_countries + 1):
                                 length(ECDC_data$filter_countries)] <- NULL
    messsage(paste("The number of countries exceeds the maximum amount",
                   "to be plotted: just the data corresponding to the first",
                   max_countries, "countries will be plotted"))
    
  }
  
  # We select random (different between them) markers between the sample
  rand_markers <- sample(plot_markers, length(ECDC_data$filter_countries),
                         replace = FALSE)
  
  # Each country is stored in a list named as "ECDC_" + country code
  n_cases <- rep(0, length(ECDC_data$filter_data))
  dates_allowed <- rep(ifelse(aligned, 0, as.Date("2019-12-31", '%Y-%m-%d')),
                       length(ECDC_data$filter_data))
  
  len_max <- 0
  for (i in 1:length(ECDC_data$filter_data)) {
    
    # If threshold = TRUE, we just consider the cases when the cumulative
    # infected is at least equal to min_cases
    n_cases[i] <- ifelse(threshold,
                         sum(ECDC_data$filter_data[[i]]$covid_data$cum_inf >
                               min_cases),
                         length(ECDC_data$filter_data[[i]]$covid_data$cum_inf))
    
    dates_allowed[i] <-
      ifelse(aligned, 0,
             ECDC_data$filter_data[[i]]$covid_data$date[n_cases[i]])
    
    ECDC_data$filter_data[[i]]$covid_data <- 
      ECDC_data$filter_data[[i]]$covid_data[
        -((n_cases[i] + 1):
            length(ECDC_data$filter_data[[i]]$covid_data$cum_inf)), ]
    
    assign(paste0("ECDC_", ECDC_data$filter_countries[i]),
           ECDC_data$filter_data[[i]])
    
    len_max <- max(len_max,
                   length(ECDC_data$filter_data[[i]]$covid_data$cum_inf))
    
  }
  
  fig1 <- fig2 <- fig3 <- fig4 <- fig5 <- fig6 <- NULL
  if (inc_inf) {
    
    for (i in 1:length(ECDC_data$filter_countries)) {
      
      if (i == 1) {
        
        fig1 <-
          plot_ly(data =
                    get(paste0("ECDC_",
                               ECDC_data$filter_countries[i]))$covid_data,
                  x = ~date, y = ~inc_inf,
                  name = ECDC_data$filter_countries[i],
                  colors = brewer.pal(n = length(ECDC_data$filter_countries),
                                      name = "RdBu"),
                  type = 'scatter', mode = 'markers+lines',
                  marker = list(symbol = rand_markers[i], size = 8.5,
                                line = list(width = 2)))
        
      }
      else {
        
        fig1 <-
          fig1 %>% add_trace(data = get(paste0("ECDC_",
                                               ECDC_data$filter_countries[i]))$covid_data,
                             x = ~date, y = ~inc_inf,
                             name = ECDC_data$filter_countries[i],
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
    fig1 <- fig1 %>%
      layout(title =
               ifelse(aligned,
                      'Daily infected (aligned, day 0 = cumul. infected > 100',
                      ifelse(threshold,
                      'Daily infected since cumulative infected > 100',
                      'Daily infected')),
             yaxis = list(title = 'new daily infected', zeroline = FALSE),
             xaxis = list(range =
                            ifelse(aligned, 0:(len_max - 1),
                                   list(min(dates_allowed),
                                        format(Sys.time(), "%Y-%m-%d"))),
                          title = 'dates', zeroline = FALSE))

  }
  
  if(inc_deaths) {
    for (i in 1:length(ECDC_data$filter_countries)) {
      
      if (i == 1) {
        
        fig2 <-
          plot_ly(data =
                    get(paste0("ECDC_",
                               ECDC_data$filter_countries[i]))$covid_data,
                  x = ~date, y = ~inc_deaths,
                  name = ECDC_data$filter_countries[i],
                  colors = brewer.pal(n = length(ECDC_data$filter_countries),
                                      name = "RdBu"),
                  type = 'scatter', mode = 'markers+lines',
                  marker = list(symbol = rand_markers[i], size = 8.5,
                                line = list(width = 2)))
        
      }
      else {
        
        fig2 <-
          fig2 %>% add_trace(data = get(paste0("ECDC_",
                                               ECDC_data$filter_countries[i]))$covid_data,
                             x = ~date, y = ~inc_deaths,
                             name = ECDC_data$filter_countries[i],
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
    fig2 <- fig2 %>%
      layout(title =
               ifelse(aligned,
                      'Daily deaths (aligned, day 0 = cumul. infected > 100',
                      ifelse(threshold,
                             'Daily deaths since cumulative infected > 100',
                             'Daily deaths')),
             yaxis = list(title = 'new daily deaths', zeroline = FALSE),
             xaxis = list(range = 
                            ifelse(aligned, 0:(len_max - 1),
                                   list(min(dates_allowed),
                                        format(Sys.time(), "%Y-%m-%d"))),
                          title = 'dates', zeroline = FALSE))

  }
  
  if(cum_inf) {
    
    for (i in 1:length(ECDC_data$filter_countries)) {
      
      if (i == 1) {
        
        fig3 <-
          plot_ly(data =
                    get(paste0("ECDC_",
                               ECDC_data$filter_countries[i]))$covid_data,
                  x = ~date, y = ~cum_inf,
                  xaxis = list(range =
                                 list(min(dates_allowed),
                                      format(Sys.time(), "%Y-%m-%d"))),
                  name = ECDC_data$filter_countries[i],
                  colors = brewer.pal(n = length(ECDC_data$filter_countries),
                                      name = "RdBu"),
                  type = 'scatter', mode = 'markers+lines',
                  marker = list(symbol = rand_markers[i], size = 8.5,
                                line = list(width = 2)))
        
      }
      else {
        
        fig3 <-
          fig3 %>% add_trace(data = get(paste0("ECDC_",
                                               ECDC_data$filter_countries[i]))$covid_data,
                             x = ~date, y = ~cum_inf,
                             name = ECDC_data$filter_countries[i],
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
    fig3 <- fig3 %>%
      layout(title =
               ifelse(aligned,
                      'Cumulative infected (aligned, day 0 = cumul. infected > 100',
                      ifelse(threshold,
                             'Cumulative infected since cumulative infected > 100',
                             'Cumulative infected')),
             yaxis = list(title = 'Cumulative infected', zeroline = FALSE),
             xaxis = list(range = 
                            ifelse(aligned, 0:(len_max - 1),
                                   list(min(dates_allowed),
                                        format(Sys.time(), "%Y-%m-%d"))),
                          title = 'dates', zeroline = FALSE))

  }
  
  if(cum_deaths) {

    for (i in 1:length(ECDC_data$filter_countries)) {
      if (i == 1) {
        
        fig4 <-
          plot_ly(data =
                    get(paste0("ECDC_",
                               ECDC_data$filter_countries[i]))$covid_data,
                  x = ~date, y = ~cum_deaths,
                  name = ECDC_data$filter_countries[i],
                  colors = brewer.pal(n = length(ECDC_data$filter_countries),
                                      name = "RdBu"),
                  type = 'scatter', mode = 'markers+lines',
                  marker = list(symbol = rand_markers[i], size = 8.5,
                                line = list(width = 2)))
        
      }
      else {
        
        fig4 <-
          fig4 %>% add_trace(data = get(paste0("ECDC_",
                                               ECDC_data$filter_countries[i]))$covid_data,
                             x = ~date, y = ~cum_deaths,
                             name = ECDC_data$filter_countries[i],
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
    fig4 <- fig4 %>%
      layout(title = 
               ifelse(aligned,
                      'Cumulative deaths (aligned, day 0 = cumul. infected > 100',
                      ifelse(threshold,
                             'Cumulative deaths since cumulative infected > 100',
                             'Cumulative deaths')),
             yaxis = list(title = 'Cumulative deaths', zeroline = FALSE),
             xaxis = list(range = 
                            ifelse(aligned, 0:(len_max - 1),
                                   list(min(dates_allowed),
                                        format(Sys.time(), "%Y-%m-%d"))),
                          title = 'dates', zeroline = FALSE))
    
  }
  
  if(growth_inf) {
    
    
    for (i in 1:length(ECDC_data$filter_countries)) {
      
      if (i == 1) {
        
        fig5 <-
          plot_ly(data =
                    get(paste0("ECDC_",
                               ECDC_data$filter_countries[i]))$covid_data,
                  x = ~date, y = ~growth_inf,
                  name = ECDC_data$filter_countries[i],
                  colors = brewer.pal(n = length(ECDC_data$filter_countries),
                                      name = "RdBu"),
                  type = 'scatter', mode = 'markers+lines',
                  marker = list(symbol = rand_markers[i], size = 8.5,
                                line = list(width = 2)))
        
      }
      else {
        
        fig5 <-
          fig5 %>% add_trace(data = get(paste0("ECDC_",
                                               ECDC_data$filter_countries[i]))$covid_data,
                             x = ~date, y = ~growth_inf,
                             name = ECDC_data$filter_countries[i],
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
    fig5 <- fig5 %>%
      layout(title =
               ifelse(aligned,
                      '% growth infected (aligned, day 0 = cumul. infected > 100',
                      ifelse(threshold,
                             '% growth infected since cumulative infected > 100',
                             '%growth infected')),
             yaxis = list(title = '% growth of infected', zeroline = FALSE),
             xaxis = list(range = 
                            ifelse(aligned, 0:(len_max - 1),
                                   list(min(dates_allowed),
                                        format(Sys.time(), "%Y-%m-%d"))),
                          title = 'dates', zeroline = FALSE))
 
  }
  
  if(growth_deaths) {
    
    for (i in 1:length(ECDC_data$filter_countries)) {
      
      if (i == 1) {
        
        fig6 <-
          plot_ly(data =
                    get(paste0("ECDC_",
                               ECDC_data$filter_countries[i]))$covid_data,
                  x = ~date, y = ~growth_deaths,
                  name = ECDC_data$filter_countries[i],
                  colors = brewer.pal(n = length(ECDC_data$filter_countries),
                                      name = "RdBu"),
                  type = 'scatter', mode = 'markers+lines',
                  marker = list(symbol = rand_markers[i], size = 8.5,
                                line = list(width = 2)))
        
      }
      else {
        
        fig6 <-
          fig6 %>% add_trace(data = get(paste0("ECDC_",
                                               ECDC_data$filter_countries[i]))$covid_data,
                             x = ~date, y = ~growth_deaths,
                             name = ECDC_data$filter_countries[i],
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
    fig6 <- fig6 %>%
      layout(title = 
               ifelse(aligned,
                      '% growth deaths (aligned, day 0 = cumul. infected > 100',
                      ifelse(threshold,
                             '% growth deaths since cumulative infected > 100',
                             '% growth deaths')),
             yaxis = list(title = '% growth of deaths', zeroline = FALSE),
             xaxis = list(range = 
                            ifelse(aligned, 0:(len_max - 1),
                                   list(min(dates_allowed),
                                        format(Sys.time(), "%Y-%m-%d"))),
                          title = 'dates', zeroline = FALSE))
    
  }
  
  if (mortality_rate) {
    
    for (i in 1:length(ECDC_data$filter_countries)) {
      
      if (i == 1) {
        
        fig7 <-
          plot_ly(data =
                    get(paste0("ECDC_",
                               ECDC_data$filter_countries[i]))$covid_data,
                  x = ~date, y = ~mortality_rate,
                  name = ECDC_data$filter_countries[i],
                  colors = brewer.pal(n = length(ECDC_data$filter_countries),
                                      name = "RdBu"),
                  type = 'scatter', mode = 'markers+lines',
                  marker = list(symbol = rand_markers[i], size = 8.5,
                                line = list(width = 2)))
        
      }
      else {
        
        fig7 <-
          fig7 %>% add_trace(data = get(paste0("ECDC_",
                                               ECDC_data$filter_countries[i]))$covid_data,
                             x = ~date, y = ~mortality_rate,
                             name = ECDC_data$filter_countries[i],
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
  fig7 <- fig7 %>%
    layout(title = 
             ifelse(aligned,
                    'Mortality rate (aligned, day 0 = cumul. infected > 100',
                    ifelse(threshold,
                           'Mortality rate since cumulative infected > 100',
                           'Mortality rate')),
           yaxis = list(title = 'Mortality rate', zeroline = FALSE),
           xaxis = list(range = 
                          ifelse(aligned, 0:(len_max - 1),
                                 list(min(dates_allowed),
                                      format(Sys.time(), "%Y-%m-%d"))),
                        title = 'dates', zeroline = FALSE))
  
}
  
  # Output figures
  return(list("fig_inc_inf" = fig1, "fig_inc_deaths" = fig2,
              "fig_cum_inf" = fig3, "fig_cum_deaths" = fig4,
              "fig_growth_inf" = fig5, "fig_growth_deaths" = fig6,
              "fig_mortality_rate" = fig7))
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

