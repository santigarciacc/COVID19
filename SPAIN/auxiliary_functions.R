

#
# Preprocessing the data from Spain: raw data is introduced as a csv
# in which the first column SHOULD BE the dates, and whose variable names
# should be provided in "var_names"
#
preproc_data_spa <- function(raw_data, var_names =
                               c("fecha", "casos", "fallecimientos", "altas")) {
  
  # Creating the data.frame just with dates
  new_data <- data.frame("fechas" =
                           levels(raw_data[, variable.names(raw_data) ==
                                             var_names[1]]))
  
  # Creating variables and replacing the first NA as 0's
  for (i in 2:length(var_names)) {
    
    # Auxiliary variables
    aux <- raw_data[, variable.names(raw_data) == var_names[i]]
    aux_cum <- replace(aux, is.na(aux), 0)
    aux_daily <- c(aux_cum[1], diff(aux_cum))
    
    # Create a variable named as the string contained in var_names[i]
    new_data <- new_data %>% mutate(replace(aux_daily, aux_daily < 0, 0)) %>%
      mutate(aux_cum)
    colnames(new_data)[2 + 2 * (i - 2)] <- paste0(var_names[i], "_diarios")
    colnames(new_data)[2 + 2 * (i - 2) + 1] <- paste0(var_names[i], "_acum")
    
  }
  
  # Output data.frame
  return(new_data)
  
}


#
# Function for plotting some curves using the raw data contained in "data", 
# since the first day of pandemic in which the (threshold)% of current cum
# cases. Population of country should be also provided to compute the
# susceptibles (albeit susceptibles are not plotted if suscep = FALSE). If data
# by sex is plotted (by_sex = TRUE), data_male and data_female should be
# provided. The daily growth (%) of SIR variables can be also computed if
# plot_vel = TRUE
#
plot_descriptive_curves <-
  function(data, population, data_male = NULL, data_female = NULL,
           data_by_age_sex = NULL,
           suscep = FALSE, threshold = 0.003, ma_lag = 7,
           by_sex = FALSE, plot_vel = TRUE, plot_pop_pyram = TRUE,
           pop_pyram_by_sex = FALSE) {
    
    # Filtering since cumulative cases are greater than (threshold*100)% of
    # current cum cases
    filter_data <-
      data[data$casos_acum > threshold * rev(data$casos_acum)[1], ]
    
    # ACTIVE CASES (I) = cumulative cases - [cum discharges + cum deaths]
    actives <- filter_data$casos_acum - filter_data$altas_acum -
      filter_data$fallecimientos_acum
    
    # RECOVERED (R) = cumulative discharges + cumulative deaths
    recovered <- filter_data$altas_acum + filter_data$fallecimientos_acum
    
    # SUSCEPTIBLES (S) = population - recovered - actives - deaths
    susceptibles <- population$total - recovered - actives -
      filter_data$fallecimientos_acum
    
    # Creating a new axis scale if we plot susceptible
    ay <- list(overlaying = "y", side = "left", title = "Scale of susceptibles")
    
    # Figure of raw data
    fig_raw_data <- plot_ly()
    if (suscep) {
      
      fig_raw_data <-
        fig_raw_data %>% add_bars(x = filter_data$fechas, y = susceptibles,
                                  name = 'Susceptibles',
                                  marker =
                                    list(color = 'rgba(74, 2226, 150, 0.8)',
                                         line = list(color = 'rgba(33, 5, 75, 1)',
                                                     width = 1.5)),
                                  offsetgroup = 1, yaxis = "y2")
      
    }
    
    fig_raw_data <-
      fig_raw_data %>% add_bars(x = filter_data$fechas, y = actives,
                                name = 'Confirmed active cases',
                                marker =
                                  list(color = 'rgba(231, 208, 3, 0.8)',
                                       line = list(color = 'rgba(33, 5, 75, 1)',
                                                   width = 1.5)),
                                offsetgroup = suscep + 1, yaxis = "y")
    fig_raw_data <-
      fig_raw_data %>% add_bars(x = filter_data$fechas, y = recovered,
                                name = 'Confirmed recovered',
                                marker =
                                  list(color = 'rgba(114, 178, 242, 0.8)',
                                       line = list(color = 'rgba(33, 5, 75, 1)',
                                                   width = 1.3)),
                                offsetgroup = suscep + 2, yaxis = "y")
    fig_raw_data <-
      fig_raw_data %>% add_bars(x = filter_data$fechas,
                                y = filter_data$fallecimientos_acum,
                                name = 'Confirmed deaths',
                                marker =
                                  list(color = 'rgba(232, 93, 95, 0.8)',
                                       line = list(color = 'rgba(33, 5, 75, 1)',
                                                   width = 1.5)),
                                offsetgroup = suscep + 3, yaxis = "y")
    fig_raw_data <-
      fig_raw_data %>% add_bars(x = filter_data$fechas,
                                y = filter_data$altas_acum,
                                name = 'Confirmed discharges',
                                marker =
                                  list(color = 'rgba(5, 81, 13, 0.8)',
                                       line = list(color = 'rgba(33, 5, 75, 1)',
                                                   width = 1.5)),
                                offsetgroup = suscep + 4, yaxis = "y")
    
    # Layout
    fig_raw_data <- fig_raw_data %>%
      layout(yaxis2 = ay,
             title = paste0("Current SIR model in Spain (no predictive), ",
                            "source: Datadista Github repository\n",
                            "Graphics by Javier Álvarez Liébana"),
             xaxis = list(title = "Dates",
                          tickfont = list(size = 14,
                                          color = 'rgb(107, 107, 107)'),
                          tickangle = -45),
             yaxis = list(title = "Scale of population ",
                          titlefont = list(size = 16,
                                           color = 'rgb(107, 107, 107)'),
                          tickfont = list(size = 14,
                                          color = 'rgb(107, 107, 107)'),
                          side = "right"),
             legend = list(bgcolor = I('gray70'),
                           bordercolor = 'rgba(255, 255, 255, 1)',
                           x = 1.1, y = 1), barmode = 'group', bargap = 0.08,
             margin = list(b = 100)) %>%
      add_annotations(bgcolor = 'rgba(126, 235, 148, 0.25)',
                      x = 0.01, y = 0.99, text = "NOTES:",
                      showarrow = FALSE, xref = 'paper', yref = 'paper',
                      font = list(color = 'rgba(8, 8, 8, 1)',
                                  size = 18))  %>%
      add_annotations(bgcolor = 'rgba(126, 235, 148, 0.25)',
                      x = 0.07, y = 0.9,
                      text = "09/03: closing schools in Madrid",
                      showarrow = FALSE, xref = 'paper', yref = 'paper',
                      font = list(color = 'rgba(8, 8, 8, 1)',
                                  size = 12))  %>%
      add_annotations(bgcolor = 'rgba(126, 235, 148, 0.25)',
                      x = 0.07, y = 0.84,
                      text = "10/03: closing schools in Vitoria",
                      showarrow = FALSE, xref = 'paper', yref = 'paper',
                      font = list(color = 'rgba(8, 8, 8, 1)',
                                  size = 12))  %>%
      add_annotations(bgcolor = 'rgba(126, 235, 148, 0.25)',
                      x = 0.07, y = 0.78,
                      text = "10/03: cancelled flights from Italy",
                      showarrow = FALSE, xref = 'paper', yref = 'paper',
                      font = list(color = 'rgba(8, 8, 8, 1)',
                                  size = 12))  %>%
      add_annotations(bgcolor = 'rgba(126, 235, 148, 0.25)',
                      x = 0.07, y = 0.72,
                      text = "14/03: lockdown",
                      showarrow = FALSE, xref = 'paper', yref = 'paper',
                      font = list(color = 'rgba(8, 8, 8, 1)',
                                  size = 12))  %>%
      add_annotations(bgcolor = 'rgba(126, 235, 148, 0.25)',
                      x = 0.07, y = 0.635,
                      text = "28/03: extreme lockdown: only essential workers",
                      showarrow = FALSE, xref = 'paper', yref = 'paper',
                      font = list(color = 'rgba(8, 8, 8, 1)',
                                  size = 12))  %>%
      add_annotations(bgcolor = 'rgba(126, 235, 148, 0.25)',
                      x = 0.07, y = 0.57,
                      text = "13/04: normal lockdown again",
                      showarrow = FALSE, xref = 'paper', yref = 'paper',
                      font = list(color = 'rgba(8, 8, 8, 1)',
                                  size = 12)) 
    
    
    ##
    ## Smoothed graphics for daily data
    ##
    
    # Filtering since cumulative cases are greater than 200
    filter_data2 <- data[data$casos_acum > 200, ]
    
    # Smoothing moving average: daily cases
    variable <- filter_data2$casos_diarios
    smooth_ma_cases <- rep(0, length(variable))
    for (i in ma_lag:length(variable)) {
      
      smooth_ma_cases[i] <- mean(variable[(i - ma_lag + 1):i])
      
    }
    
    # Smoothing moving average: discharges
    variable <- filter_data2$altas_diarios
    smooth_ma_discharges <- rep(0, length(variable))
    for (i in ma_lag:length(variable)) {
      
      smooth_ma_discharges[i] <- mean(variable[(i - ma_lag + 1):i])
      
    }
    
    # Smoothing moving average: daily deaths
    variable <- filter_data2$fallecimientos_diarios
    smooth_ma_deaths <- rep(0, length(variable))
    for (i in ma_lag:length(variable)) {
      
      smooth_ma_deaths[i] <- mean(variable[(i - ma_lag + 1):i])
      
    }

    
    fig2_raw_data <- plot_ly()
    fig2_raw_data <-
      fig2_raw_data %>%
      add_bars(x = filter_data2$fechas, y = filter_data2$casos_diarios,
               name = 'New (confirmed) cases',
               marker = list(color = 'rgba(231, 208, 3, 0.7)',
                             line = list(color = 'rgba(33, 5, 75, 1)',
                                         width = 1.5)),
               offsetgroup = 1, yaxis = "y")
    
    fig2_raw_data <-
      fig2_raw_data %>%
      add_bars(x = filter_data2$fechas, y = filter_data2$altas_diarios,
               name = 'New (confirmed) discharges',
               marker = list(color = 'rgba(114, 178, 242, 0.8)',
                             line = list(color = 'rgba(33, 5, 75, 1)',
                                         width = 1.3)),
               offsetgroup = 2, yaxis = "y")
    
    fig2_raw_data <-
      fig2_raw_data %>%
      add_bars(x = filter_data2$fechas, y = filter_data2$fallecimientos_diarios,
               name = 'New (confirmed) deaths',
               marker = list(color = 'rgba(232, 93, 95, 0.8)',
                             line = list(color = 'rgba(33, 5, 75, 1)',
                                         width = 1.5)),
               offsetgroup = 3, yaxis = "y")
    
    # Layout
    fig2_raw_data <- fig2_raw_data %>%
      layout(title =
               paste0("New daily cases, deaths and discharges in Spain, ",
                      "source: Datadista Github repository\n",
                      "Graphics by Javier Álvarez Liébana"),
             xaxis = list(title = "Dates",
                          tickfont = list(size = 14,
                                          color = 'rgb(107, 107, 107)'),
                          tickangle = -45),
             yaxis = list(title = "Amount of people",
                          titlefont = list(size = 16,
                                           color = 'rgb(107, 107, 107)'),
                          tickfont = list(size = 14,
                                          color = 'rgb(107, 107, 107)'),
                          side = "right"),
             legend = list(bgcolor = I('gray70'),
                           bordercolor = 'rgba(255, 255, 255, 1)',
                           x = 1.1, y = 1), barmode = 'group', bargap = 0.08,
             margin = list(b = 100)) %>%
      add_annotations(bgcolor = 'rgba(126, 235, 148, 0.25)',
                      x = 0.005, y = 0.99, text = "NOTES:",
                      showarrow = FALSE, xref = 'paper', yref = 'paper',
                      font = list(color = 'rgba(8, 8, 8, 1)',
                                  size = 18))  %>%
      add_annotations(bgcolor = 'rgba(126, 235, 148, 0.25)',
                      x = 0.015, y = 0.9,
                      text = "09/03: closing schools in Madrid",
                      showarrow = FALSE, xref = 'paper', yref = 'paper',
                      font = list(color = 'rgba(8, 8, 8, 1)',
                                  size = 12))  %>%
      add_annotations(bgcolor = 'rgba(126, 235, 148, 0.25)',
                      x = 0.015, y = 0.84,
                      text = "10/03: closing schools in Vitoria",
                      showarrow = FALSE, xref = 'paper', yref = 'paper',
                      font = list(color = 'rgba(8, 8, 8, 1)',
                                  size = 12))  %>%
      add_annotations(bgcolor = 'rgba(126, 235, 148, 0.25)',
                      x = 0.015, y = 0.78,
                      text = "10/03: cancelled flights from Italy",
                      showarrow = FALSE, xref = 'paper', yref = 'paper',
                      font = list(color = 'rgba(8, 8, 8, 1)',
                                  size = 12))  %>%
      add_annotations(bgcolor = 'rgba(126, 235, 148, 0.25)',
                      x = 0.015, y = 0.72,
                      text = "14/03: lockdown",
                      showarrow = FALSE, xref = 'paper', yref = 'paper',
                      font = list(color = 'rgba(8, 8, 8, 1)',
                                  size = 12))  %>%
      add_annotations(bgcolor = 'rgba(126, 235, 148, 0.25)',
                      x = 0.015, y = 0.635,
                      text = "28/03: extreme lockdown: just essential workers",
                      showarrow = FALSE, xref = 'paper', yref = 'paper',
                      font = list(color = 'rgba(8, 8, 8, 1)',
                                  size = 12))  %>%
      add_annotations(bgcolor = 'rgba(126, 235, 148, 0.25)',
                      x = 0.015, y = 0.57,
                      text = "13/04: normal lockdown again",
                      showarrow = FALSE, xref = 'paper', yref = 'paper',
                      font = list(color = 'rgba(8, 8, 8, 1)',
                                  size = 12)) 
    
    
    fig3_raw_data <- plot_ly()
    fig3_raw_data <-
      fig3_raw_data %>%
      add_bars(x = filter_data2$fechas, y = smooth_ma_cases,
               name = paste0("Smoothing MA ", ma_lag, "-days: cases"),
               marker = list(color = 'rgba(231, 208, 3, 0.7)',
                             line = list(color = 'rgba(33, 5, 75, 1)',
                                         width = 1.5)),
               offsetgroup = 1, yaxis = "y")
    
    fig3_raw_data <-
      fig3_raw_data %>%
      add_bars(x = filter_data2$fechas, y = smooth_ma_discharges,
               name = paste0("Smoothing MA ", ma_lag, "-days: discharges"),
               marker = list(color = 'rgba(114, 178, 242, 0.8)',
                             line = list(color = 'rgba(33, 5, 75, 1)',
                                         width = 1.3)),
               offsetgroup = 2, yaxis = "y")
    
    fig3_raw_data <-
      fig3_raw_data %>%
      add_bars(x = filter_data2$fechas,  y = smooth_ma_deaths,
               name = paste0("Smoothing MA ", ma_lag, "-days: deaths"),
               marker = list(color = 'rgba(232, 93, 95, 0.8)',
                             line = list(color = 'rgba(33, 5, 75, 1)',
                                         width = 1.5)),
               offsetgroup = 3, yaxis = "y")
    
    # Layout
    fig3_raw_data <- fig3_raw_data %>%
      layout(title =
               paste0("New daily cases, deaths and discharges in Spain with ",
                      "MA ", ma_lag, "-days smoothing, ",
                      "source: Datadista Github repository\n",
                      "Graphics by Javier Álvarez Liébana"),
             xaxis = list(title = "Dates",
                          tickfont = list(size = 14,
                                          color = 'rgb(107, 107, 107)'),
                          tickangle = -45),
             yaxis = list(title = "Amount of people",
                          titlefont = list(size = 16,
                                           color = 'rgb(107, 107, 107)'),
                          tickfont = list(size = 14,
                                          color = 'rgb(107, 107, 107)'),
                          side = "right"),
             legend = list(bgcolor = I('gray70'),
                           bordercolor = 'rgba(255, 255, 255, 1)',
                           x = 1.1, y = 1), barmode = 'group', bargap = 0.08,
             margin = list(b = 100)) %>%
      add_annotations(bgcolor = 'rgba(126, 235, 148, 0.25)',
                      x = 0.005, y = 0.99, text = "NOTES:",
                      showarrow = FALSE, xref = 'paper', yref = 'paper',
                      font = list(color = 'rgba(8, 8, 8, 1)',
                                  size = 18))  %>%
      add_annotations(bgcolor = 'rgba(126, 235, 148, 0.25)',
                      x = 0.015, y = 0.9,
                      text = "09/03: closing schools in Madrid",
                      showarrow = FALSE, xref = 'paper', yref = 'paper',
                      font = list(color = 'rgba(8, 8, 8, 1)',
                                  size = 12))  %>%
      add_annotations(bgcolor = 'rgba(126, 235, 148, 0.25)',
                      x = 0.015, y = 0.84,
                      text = "10/03: closing schools in Vitoria",
                      showarrow = FALSE, xref = 'paper', yref = 'paper',
                      font = list(color = 'rgba(8, 8, 8, 1)',
                                  size = 12))  %>%
      add_annotations(bgcolor = 'rgba(126, 235, 148, 0.25)',
                      x = 0.015, y = 0.78,
                      text = "10/03: cancelled flights from Italy",
                      showarrow = FALSE, xref = 'paper', yref = 'paper',
                      font = list(color = 'rgba(8, 8, 8, 1)',
                                  size = 12))  %>%
      add_annotations(bgcolor = 'rgba(126, 235, 148, 0.25)',
                      x = 0.015, y = 0.72,
                      text = "14/03: lockdown",
                      showarrow = FALSE, xref = 'paper', yref = 'paper',
                      font = list(color = 'rgba(8, 8, 8, 1)',
                                  size = 12))  %>%
      add_annotations(bgcolor = 'rgba(126, 235, 148, 0.25)',
                      x = 0.015, y = 0.635,
                      text = "28/03: extreme lockdown: just essential workers",
                      showarrow = FALSE, xref = 'paper', yref = 'paper',
                      font = list(color = 'rgba(8, 8, 8, 1)',
                                  size = 12))  %>%
      add_annotations(bgcolor = 'rgba(126, 235, 148, 0.25)',
                      x = 0.015, y = 0.57,
                      text = "13/04: normal lockdown again",
                      showarrow = FALSE, xref = 'paper', yref = 'paper',
                      font = list(color = 'rgba(8, 8, 8, 1)',
                                  size = 12)) 
    
    
    # Creating a new axis scale if we plot susceptible
    ay2 <- list(overlaying = "y", side = "left",
               title = "Scale of mortality rate")
    
    # Figure of raw data
    fig4_raw_data <- plot_ly()
    fig4_raw_data <-
        fig4_raw_data %>%
      add_trace(x = filter_data2$fechas,
                y = filter_data2$fallecimientos_acum / population$total,
                name = "Mortality rate",
                type = "scatter", mode = "markers+lines",
                line = list(color = 'rgba(47, 152, 5, 1)'),
                marker = list(color = 'rgba(232, 93, 95, 0.8)',
                              size = 9,
                              line = list(color = 'rgba(33, 5, 75, 1)',
                                          width = 2)), yaxis = "y2")
    
    fig4_raw_data <-
      fig4_raw_data %>%
      add_trace(x = filter_data2$fechas,
                y = filter_data2$fallecimientos_acum / filter_data2$casos_acum,
                name = "Fatality rate",
                type = "scatter", mode = "markers+lines",
                line = list(color = 'rgba(110, 5, 152, 1)'),
                marker = list(color = 'rgba(7, 139, 176, 0.8)',
                              size = 9,
                              line = list(color = 'rgba(33, 5, 75, 1)',
                                          width = 2)), yaxis = "y")
    
    # Layout
    fig4_raw_data <- fig4_raw_data %>%
      layout(yaxis2 = ay2,
             title =
               paste0("Mortality and fatality rates in Spain (no predictive), ",
                      "source: Datadista Github repository\n",
                      "Graphics by Javier Álvarez Liébana"),
             xaxis = list(title = "Dates",
                          tickfont = list(size = 14,
                                          color = 'rgb(107, 107, 107)'),
                          tickangle = -45),
             yaxis = list(title = "Scale of fatality rate",
                          titlefont = list(size = 16,
                                           color = 'rgb(107, 107, 107)'),
                          tickfont = list(size = 14,
                                          color = 'rgb(107, 107, 107)'),
                          side = "right"),
             legend = list(bgcolor = I('gray70'),
                           bordercolor = 'rgba(255, 255, 255, 1)',
                           x = 1.1, y = 1)) %>%
      add_annotations(bgcolor = 'rgba(126, 235, 148, 0.25)',
                      x = 0.01, y = 0.99, text = "NOTES:",
                      showarrow = FALSE, xref = 'paper', yref = 'paper',
                      font = list(color = 'rgba(8, 8, 8, 1)',
                                  size = 18))  %>%
      add_annotations(bgcolor = 'rgba(126, 235, 148, 0.25)',
                      x = 0.07, y = 0.9,
                      text = "09/03: closing schools in Madrid",
                      showarrow = FALSE, xref = 'paper', yref = 'paper',
                      font = list(color = 'rgba(8, 8, 8, 1)',
                                  size = 12))  %>%
      add_annotations(bgcolor = 'rgba(126, 235, 148, 0.25)',
                      x = 0.07, y = 0.84,
                      text = "10/03: closing schools in Vitoria",
                      showarrow = FALSE, xref = 'paper', yref = 'paper',
                      font = list(color = 'rgba(8, 8, 8, 1)',
                                  size = 12))  %>%
      add_annotations(bgcolor = 'rgba(126, 235, 148, 0.25)',
                      x = 0.07, y = 0.78,
                      text = "10/03: cancelled flights from Italy",
                      showarrow = FALSE, xref = 'paper', yref = 'paper',
                      font = list(color = 'rgba(8, 8, 8, 1)',
                                  size = 12))  %>%
      add_annotations(bgcolor = 'rgba(126, 235, 148, 0.25)',
                      x = 0.07, y = 0.72,
                      text = "14/03: lockdown",
                      showarrow = FALSE, xref = 'paper', yref = 'paper',
                      font = list(color = 'rgba(8, 8, 8, 1)',
                                  size = 12))  %>%
      add_annotations(bgcolor = 'rgba(126, 235, 148, 0.25)',
                      x = 0.07, y = 0.635,
                      text = "28/03: extreme lockdown: only essential workers",
                      showarrow = FALSE, xref = 'paper', yref = 'paper',
                      font = list(color = 'rgba(8, 8, 8, 1)',
                                  size = 12))  %>%
      add_annotations(bgcolor = 'rgba(126, 235, 148, 0.25)',
                      x = 0.07, y = 0.57,
                      text = "13/04: normal lockdown again",
                      showarrow = FALSE, xref = 'paper', yref = 'paper',
                      font = list(color = 'rgba(8, 8, 8, 1)',
                                  size = 12)) 
    
    # graphics of actives and deaths by sex (discharges are not available by sex)
    fig_by_sex <- NULL
    if (by_sex) {
      
      if (is.null(data_male) | is.null(data_female)) {
        
        stop("Data disaggregated by sex was not provided")
        
      }
      
      idx <- which(levels(data_male$fechas) == levels(filter_data$fechas)[1])[1]
      idx <- ifelse(is.na(idx), 1, idx)
      filter_data_male <- data_male[idx:length(data_male$fechas), ]
      filter_data_female <- data_female[idx:length(data_female$fechas), ]
      
      fig1 <- plot_ly()
      fig1 <- fig1 %>% add_bars(x = filter_data_male$fechas,
                                y = filter_data_male$casos_confirmados_acum,
                                showlegend = FALSE,
                                marker = list(color = 'rgba(240, 58, 10, 0.8)',
                                              line = list(color = 'rgba(33, 5, 75, 1)',
                                                          width = 1.5)),
                                offsetgroup = 1)
      fig1 <- fig1 %>% add_bars(x = filter_data_female$fechas,
                                y = filter_data_female$casos_confirmados_acum,
                                showlegend = FALSE,
                                marker = list(color = 'rgba(90, 10, 190, 0.8)',
                                              line = list(color = 'rgba(33, 5, 75, 1)',
                                                          width = 1.5)),
                                offsetgroup = 1)
      
      fig2 <- plot_ly()
      fig2 <- fig2 %>% add_bars(x = filter_data_male$fechas,
                                y = filter_data_male$fallecidos_acum,
                                legendgroup = 'group1', name = "Male",
                                marker = list(color = 'rgba(240, 58, 10, 0.8)',
                                              line = list(color = 'rgba(33, 5, 75, 1)',
                                                          width = 1.5)),
                                offsetgroup = 2)
      fig2 <- fig2 %>% add_bars(x = filter_data_female$fechas,
                                y = filter_data_female$fallecidos_acum,
                                legendgroup = 'group2', name = "Female",
                                marker = list(color = 'rgba(90, 10, 190, 0.8)',
                                              line = list(color = 'rgba(33, 5, 75, 1)',
                                                          width = 1.5)),
                                offsetgroup = 2)
      
      fig_by_sex <- subplot(fig1, fig2, shareY = FALSE) %>%
        layout(legend = c("male", "femaele"), barmode = 'stack',
               title = paste0("Cumulative cases and deaths by sex in Spain",
                              "\nSource: Datadista Github repository, ",
                              "graphics by Javier Álvarez Liébana\n\n"),
               bargap = 0.15)  %>%
        add_annotations(bgcolor = 'rgba(245, 222, 19, 0.2)',
                        x = 0.15 , y = 0.8, text = "Cumulative cases",
                        showarrow = TRUE, xref = 'paper', yref = 'paper',
                        arrowcolor = I('brown'), 
                        font = list(color = 'rgba(21, 213, 139, 0.8)',
                                    size = 18))  %>%
        add_annotations(bgcolor = 'rgba(245, 222, 19, 0.2)',
                        x = 0.83, y = 0.8, text = "Cumulative deaths",
                        showarrow = TRUE, xref = 'paper', yref = 'paper',
                        arrowcolor = I('brown'),
                        font = list(color = 'rgba(21, 213, 139, 0.8)',
                                    size = 18))  %>%
        add_annotations(bgcolor = 'rgba(126, 235, 148, 0.25)',
                        x = 0.55, y = 0.85, text = "NOTES:",
                        showarrow = FALSE, xref = 'paper', yref = 'paper',
                        font = list(color = 'rgba(8, 8, 8, 1)',
                                    size = 16)) %>%
        add_annotations(bgcolor = 'rgba(126, 235, 148, 0.25)',
                        x = 0.65, y = 0.77,
                        text = paste0("Data by sex is not completely reported:\n",
                                      "some invidivuals are not included"),
                        showarrow = FALSE, xref = 'paper', yref = 'paper',
                        font = list(color = 'rgba(8, 8, 8, 1)',
                                    size = 11)) %>%
        add_annotations(bgcolor = 'rgba(126, 235, 148, 0.25)',
                        x = 0.65, y = 0.63,
                        text = paste0("Data by sex is not correctly reported:\n",
                                      "some cumulatives are degressive"),
                        showarrow = FALSE, xref = 'paper', yref = 'paper',
                        font = list(color = 'rgba(8, 8, 8, 1)',
                                    size = 11))
      
      
    }
    
    fig_vel <- NULL
    if(plot_vel) {
      
      vel_actives <- c(0, actives[2:length(actives)] /
                         actives[1:(length(actives) - 1)]) - 1
      vel_actives <-
        replace(vel_actives, is.na(vel_actives) | is.infinite(vel_actives), 0)
      
      vel_discharges <-
        c(0, filter_data$altas_acum[2:length(filter_data$altas_acum)] /
            filter_data$altas_acum[1:(length(filter_data$altas_acum) - 1)]) - 1
      vel_discharges <-
        replace(vel_discharges, is.na(vel_discharges) | 
                  is.infinite(vel_discharges), 0)
      
      vel_recovered <- c(0, recovered[2:length(recovered)] /
                           recovered[1:(length(recovered) - 1)]) - 1
      vel_recovered <-
        replace(vel_recovered, is.na(vel_recovered) |
                  is.infinite(vel_recovered), 0)
      
      vel_deaths <-
        c(0, filter_data$fallecimientos_acum[
          2:length(filter_data$fallecimientos_acum)] /
            filter_data$fallecimientos_acum[
              1:(length(filter_data$fallecimientos_acum) - 1)]) - 1
      vel_deaths <-
        replace(vel_deaths, is.na(vel_deaths) |
                  is.infinite(vel_deaths), 0)
      
      
      fig_vel <- plot_ly(x = filter_data$fechas, y = vel_actives, type = 'scatter',
                         mode = 'lines', name = 'Daily growth (%) of active cases',
                         fill = 'tozeroy', fillcolor = 'rgba(231, 208, 3, 0.3)',
                         line = list(color = 'rgba(231, 208, 3, 0.9)'))
      fig_vel <-
        fig_vel %>% add_trace(x = filter_data$fechas, y = vel_recovered,
                              name = 'Daily growth (%) of recovered',
                              fill = 'tozeroy',
                              fillcolor = 'rgba(114, 178, 242, 0.3)',
                              line = list(color = 'rgba(114, 178, 242, 0.3)'))
      fig_vel <-
        fig_vel %>% add_trace(x = filter_data$fechas, y = vel_deaths,
                              name = 'Daily growth (%) of deaths',
                              fill = 'tozeroy',
                              fillcolor = 'rgba(232, 93, 95, 0.3)',
                              line = list(color = 'rgba(232, 93, 95, 0.3)'))
      fig_vel <-
        fig_vel %>% add_trace(x = filter_data$fechas, y = vel_discharges,
                              name = 'Daily growth (%) of discharges',
                              fill = 'tozeroy',
                              fillcolor = 'rgba(5, 81, 13, 0.3)',
                              line = list(color = 'rgba(5, 81, 13, 0.3)'))
      
      fig_vel <- fig_vel %>%
        layout(title = paste0("Daily growth (%) in Spain (no predictive), ",
                              "source: Datadista Github repository\n",
                              "Graphics by Javier Álvarez Liébana"),
               xaxis = list(title = "Dates",
                            tickfont = list(size = 14,
                                            color = 'rgb(107, 107, 107)'),
                            tickangle = -45),
               yaxis = list(title = "Daily growth (%)",
                            titlefont = list(size = 16,
                                             color = 'rgb(107, 107, 107)'),
                            tickfont = list(size = 14,
                                            color = 'rgb(107, 107, 107)')),
               legend = list(bgcolor = I('gray70'),
                             bordercolor = 'rgba(255, 255, 255, 1)',
                             x = 1.1, y = 1))
      
    }
    
    # Population pyramids
    fig_pop_pyram_cases <- NULL
    fig_pop_pyram_deaths <- NULL
    if (plot_pop_pyram) {
      
      # Remove unuseful levels
      data_by_age_sex <-
        data_by_age_sex[data_by_age_sex$sexo != "ambos" &
                          data_by_age_sex$rango_edad != "Total" &
                          data_by_age_sex$rango_edad != "80 y +", ]
      
      # Extract dates and ages
      dates <- unique(data_by_age_sex$fecha)
      data_by_age_sex$rango_edad <- factor(data_by_age_sex$rango_edad)
      ages <- unique(data_by_age_sex$rango_edad)
      
      # Recombine the data.frame including all data for a date first
      data_by_ages <- data.frame()
      for (i in 1:length(ages)) {
        
        data_by_ages <-
          rbind(data_by_ages,
                data_by_age_sex[data_by_age_sex$rango_edad == ages[i], ])
        
      }
      
      # Rename the levels
      data_by_ages$sexo <- factor(data_by_ages$sexo)
      levels(data_by_ages$sexo) <- c("male", "female")
      
      # Remove unuseful variables
      data_by_ages$hospitalizados <- NULL
      data_by_ages$ingresos_uci <- NULL
      
      # Compute negative values for male, positive values for female
      data_by_ages$casos_sign <- data_by_ages$casos_confirmados
      data_by_ages$casos_sign[data_by_ages$sexo == "male"] <- -1 *
        data_by_ages$casos_confirmados[data_by_ages$sexo == "male"]
      
      data_by_ages$fallecidos_sign <-data_by_ages$fallecidos
      data_by_ages$fallecidos_sign[data_by_ages$sexo == "male"] <- -1 *
        data_by_ages$fallecidos[data_by_ages$sexo == "male"]
      
      for (d in 1:length(data_by_ages$fecha)) {
        
        data_by_ages$casos_acum[d] <-
          data$casos_acum[as.character(data$fechas) ==
                            as.character(data_by_ages$fecha)[d]]
        data_by_ages$fallecidos_acum[d] <-
          data$fallecimientos_acum[as.character(data$fechas) ==
                                     as.character(data_by_ages$fecha)[d]]
        
        if (data_by_ages$sexo[d] == "male") {
          
          aux <-
            data_male$casos_confirmados_acum[as.character(data_male$fechas) ==
                                               as.character(data_by_ages$fecha)[d]]
          aux2 <-
            data_male$fallecidos_acum[as.character(data_male$fechas) ==
                                        as.character(data_by_ages$fecha)[d]]
          
        } else {
          
          aux <-
            data_female$casos_confirmados_acum[as.character(data_female$fechas) ==
                                                 as.character(data_by_ages$fecha)[d]]
          aux2 <-
            data_female$fallecidos_acum[as.character(data_female$fechas) ==
                                          as.character(data_by_ages$fecha)[d]]
          
        }
        
        data_by_ages$casos_acum_sexo[d] <- aux
        data_by_ages$fallecidos_acum_sexo[d] <- aux2
      }
      data_by_ages$date <- data_by_ages$fecha
      
      if (pop_pyram_by_sex) {
        
        range <- c(-25, 25)
        dtick <- 2
        tickvals <- c(-rev(round(seq(0, 25, by = 2), 2)),
                     round(seq(0, 25, by = 2), 2))
        ticktext <-
          as.character(c(rev(round(seq(0, 25, by = 2), 2)),
                         round(seq(0, 25, by = 2), 2)))
        pop <- data_by_ages$casos_acum_sexo
        
      } else {
        
        range <- c(-7.5, 7.5)
        dtick <- 0.75
        tickvals <- c(-rev(round(seq(0, 7.5, by = 0.75), 2)),
                      round(seq(0, 7.5, by = 0.75), 2))
        ticktext <- as.character(c(rev(round(seq(0, 7.5, by = 0.75), 2)),
                                   round(seq(0, 7.5, by = 0.75), 2)))
        pop <- data_by_ages$casos_acum
        
      }
      
      fig_pop_pyram_cases <- plot_ly()
      fig_pop_pyram_cases <-
        fig_pop_pyram_cases %>%
        add_bars(data = data_by_ages,
                 x = data_by_ages$casos_sign * 100 / pop,
                 y = ~rango_edad, color = ~sexo, frame = ~date,
                 orientation = 'h', hoverinfo = 'text',
                 text = ~casos_confirmados) %>%
        layout(title = paste0("% cum. cases by sex/ages ",
                              "(resp. ", ifelse(pop_pyram_by_sex, "sex",
                                                "total"), ") in Spain. ",
                              "Source: Datadista Github repository. ",
                              "Graphics by J. Álvarez Liébana"),
               bargap = 0.1, barmode = 'overlay',
               xaxis = list(title = "% resp. of cum. cases",
                            range = range, dtick = dtick,
                            tickvals = tickvals, ticktext = ticktext,
                            tickfont = list(size = 15,
                                            color = 'rgb(107, 107, 107)'),
                            tickangle = -45),
               yaxis = list(title = "Ages"))
      
      
      if (pop_pyram_by_sex) {
        
        range <- c(-50, 50)
        dtick <- 5
        tickvals <- c(-rev(round(seq(0, 50, by = 5), 2)),
                      round(seq(0, 50, by = 5), 2))
        ticktext <-
          as.character(c(rev(round(seq(0, 50, by = 5), 2)),
                         round(seq(0, 50, by = 5), 2)))
        pop <- data_by_ages$fallecidos_acum_sexo
        
      } else {
        
        range <- c(-15, 15)
        dtick <- 2
        tickvals <- c(-rev(round(seq(0, 15, by = 2), 2)),
                      round(seq(0, 15, by = 2), 2))
        ticktext <-
          as.character(c(rev(round(seq(0, 15, by = 2), 2)),
                         round(seq(0, 15, by = 2), 2)))
        pop <- data_by_ages$fallecidos_acum
        
      }
      
      fig_pop_pyram_deaths <- plot_ly()
      fig_pop_pyram_deaths <-
        fig_pop_pyram_deaths %>%
        add_bars(data = data_by_ages,
                 x = data_by_ages$fallecidos_sign * 100 / pop,
                 y = ~rango_edad, color = ~sexo, frame = ~date,
                 orientation = 'h', hoverinfo = 'text',
                 text = ~casos_confirmados) %>%
        layout(title = paste0("% cum. deaths by sex/ages ",
                              "(resp. ", ifelse(pop_pyram_by_sex, "sex",
                                                "total"), ") in Spain. ",
                              "Source: Datadista Github repository. ",
                              "Graphics by J. Álvarez Liébana"),
               bargap = 0.1, barmode = 'overlay',
               xaxis = list(title = "% resp. of cum. deaths",
                            range = range, dtick = dtick,
                            tickvals = tickvals, ticktext = ticktext,
                            tickfont = list(size = 15,
                                            color = 'rgb(107, 107, 107)'),
                            tickangle = -45),
               yaxis = list(title = "Ages"))
      
    }
    
    # Output
    return(list("fig_SIR_data" = fig_raw_data, "fig_daily_data" = fig2_raw_data,
                "fig_smooth_daily_data" = fig3_raw_data,
                "fig_fat_mort_data" = fig4_raw_data,
                "fig_by_sex" = fig_by_sex, "fig_vel" = fig_vel,
                "fig_pop_pyram_cases" = fig_pop_pyram_cases,
                "fig_pop_pyram_deaths" = fig_pop_pyram_deaths))
    
}


##
## Function to provide some descriptive analysis and graphics about Spain data
##
desc_analysis_spa_data <- function(url_raw_data, url_data_by_age_sex,
                                   var_names_raw_data, var_names_sex_age_data,
                                   population, suscep = FALSE, by_sex = TRUE,
                                   plot_vel = TRUE, ma_lag = 7,
                                   plot_pop_pyram = TRUE,
                                   pop_pyram_by_sex = FALSE) {
  
  ##
  ## LOADING DATA
  ##
  
  ##
  ## File: nacional_covdi19.csv
  ##
  ## Raw global data by dates: "$fecha" dates in format "%Y-%m-%d", "$casos"
  ##                           cumulative cases in Spain, "$altas" cumulative
  ##                           discharges (just "confirmed" recovered) in Spain,
  ##                           "$fallecimientos" cumulative deaths in Spain,
  ##                           "$hospitalizados" admissions,
  ##                           "$ingresos_uci" ICU admissions
  ##
  ## NOTE: data from Spanish Health Ministery are collected from the cases
  ##       reported by the CCAA (autonomous regions) one day before at 20:00.
  ##       The difference between days could be included late notices.
  ##
  ## NOTE: Since 8th April, Spanish Health Ministery does not provide data about
  ##       admissions and ICU admissions, since some regions were providing
  ##       cumulative data and others prevalence data.
  ##
  
  # Loading the raw updated data from Spain
  raw_data_spa <- read.csv(url_raw_data)

  # Preprocessing raw data
  data_spa <-  preproc_data_spa(raw_data_spa, var_names_raw_data)

  # Loading data by sex and age: file nacional_covid19_rango_edad.csv. 
  # Note that now "$casos" is named as "$casos_confirmados" and variable
  # "$altas" is not provided. The cumulative data displays is not consistent
  data_by_age_sex_spa <-
    read.csv(url_data_by_age_sex)

  # Creating a data file just for male data
  data_male_spa <-
    data_by_age_sex_spa[data_by_age_sex_spa$sexo == "hombres" &
                          data_by_age_sex_spa$rango_edad == "Total", ]
  data_male_spa$sexo <-
    data_male_spa$rango_edad <- NULL # Remove variables of sex and age range

  # Creating a data file just for female data
  data_female_spa <-
    data_by_age_sex_spa[data_by_age_sex_spa$sexo == "mujeres"  &
                          data_by_age_sex_spa$rango_edad == "Total", ]
  data_female_spa$sexo <-
    data_female_spa$rango_edad <- NULL # Remove variables of sex and age range

  # Preprocessing
  data_male_spa <- preproc_data_spa(data_male_spa, var_names_sex_age_data)
  data_female_spa <- preproc_data_spa(data_female_spa, var_names_sex_age_data)


  # Plotting
  figures <-
    plot_descriptive_curves(data_spa, population, data_male = data_male_spa,
                            data_female = data_female_spa,
                            data_by_age_sex = data_by_age_sex_spa,
                            suscep = suscep, by_sex = by_sex,
                            plot_vel = plot_vel, ma_lag = ma_lag,
                            plot_pop_pyram = plot_pop_pyram,
                            pop_pyram_by_sex = pop_pyram_by_sex)
  
  # Output
  return(figures)
  
}
