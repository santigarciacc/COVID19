
##
## COVID19 DATASETS: ITALY, SPAIN, GERMANY, FRANCE, EEUU, CHINA AND SOUTH KOREA
## Sources: https://elpais.com/sociedad/2020/03/24/actualidad/1585072160_493420.html
##          https://www.eldiario.es/sociedad/mapa-evolucion-coronavirus-expansion-Espana-25-marzo_0_1005099739.html#mapamundo
##          https://elpais.com/sociedad/2020/03/16/actualidad/1584379038_891570.html
##


##
## ITALY: since cumulative infected > 100 (23/02/2020)
inf_day_ita <- c(74, 93, 131, 202, 233, 240, 566, 342, 467, 586, 769, 778, 1247,
                 1492, 1797, 977, 2690, 4821, 3497, 3590, 3233, 3076, 4657, 
                 5322, 5986, 6557, 5560, 4789, 5249, 5210,
                 6153) # Infected per day
cum_inf_ita <- sum(c(2, 1, 17, 42, 93)) + cumsum(inf_day_ita) # Cum infected
  
died_day_ita <- c(3, 2, 5, 4, 8, 5, 18, 27, 28, 41, 49, 36, 133, 97,
                  168, 196, 189, 250, 175, 368, 349, 345, 475, 427, 627, 793,
                  651, 601, 743, 683, 662) # Died per day
cum_died_ita <- sum(c(1, 1, 1, 4)) + cumsum(died_day_ita) # Cum died

recov_day_ita <- c(0, 1, 1, 42, 1, 0, 37, 66, 11, 116, 138, 109, 66, 33, 102, 0,
                   321, 213, 181, 527, 369, 414, 192, 1084, 415, 1632, 952, 408,
                   894, 1036, 999) # Recovered per day
cum_recov_ita <- 1 + cumsum(recov_day_ita) # Cum recovered
  
# Demographics from https://www.istat.it/it/popolazione-e-famiglie
N_ita <- 60317000 # Total population (updated in January 2020)
birth_rate_ita <- 7.3	# Birth rate ‰ (during 2018, updated in January 2019)
mort_rate_ita <- 10.5	# Mortality rate ‰ (during 2018, updated in January 2019)
exp_life_ita <- 82.7 # Expectation of life (2019)

# ITA dataset
ita_covid19_data <-
  list("covid" = data.frame("days" = seq(as.Date("2020/02/23"), by = "day",
                               length.out = length(inf_day_ita)),
  "inf" = inf_day_ita, "cum_inf" = cum_inf_ita,
  "recov" = recov_day_ita, "cum_recov" = cum_recov_ita,
  "died" = died_day_ita, "cum_died" = cum_died_ita),
  "country" = "ITA", "N" = N_ita, "birth_rate" = birth_rate_ita,
  "mort_rate" = mort_rate_ita, "exp_life" = exp_life_ita)


##
## SPAIN: since cumulative infected > 100 (02/03/2020)
inf_day_spa <- c(36, 45, 57, 37, 141, 100, 173, 400, 622, 582, 869, 2086, 1159,
                 1407, 2144, 1806, 2162, 4053, 2447, 4964, 3394, 6368, 4749,
                 9630, 6673) # Infected per day
cum_inf_spa <- 84 +  cumsum(inf_day_spa) # Cum infected

died_day_spa <- c(0, 1, 1, 1, 2, 5, 7, 10, 8, 12, 37, 34, 18, 152, 21, 188, 101,
                  169, 215, 344, 394, 462, 514, 738, 655) # Died per day
cum_died_spa <- cumsum(died_day_spa) # Cum died

recov_day_spa <- c(0, 0, 0, 0, 0, 28, 0, 2, 0, 151, 6, 4, 324, 0, 13, 498,
                   53, 26, 481, 537, 450, 780, 439, 1573, 1648) # Recovered per day
cum_recov_spa <- 2 + cumsum(recov_day_spa) # Cum recovered

# Demographics from https://www.ine.es/dyngs/INEbase/es/operacion.htm?c=Estadistica_C&cid=1254736176951&menu=ultiDatos&idp=1254735572981
N_spa <- 47100396 # Total population (updated in January 2020)
birth_rate_spa <- 7.94 # Birth rate ‰ (during 2018, updated in January 2019)
mort_rate_spa <- 9.10	# Mortality rate ‰ (during 2018, updated in January 2019)
exp_life_spa <- 83.19 # Expectation of life (2019)

# SPA dataset
spa_covid19_data <-
  list("covid" = data.frame("days" = seq(as.Date("2020/03/02"), by = "day",
                                         length.out = length(inf_day_spa)),
                            "inf" = inf_day_spa, "cum_inf" = cum_inf_spa,
                            "recov" = recov_day_spa, "cum_recov" = cum_recov_spa,
                            "died" = died_day_spa, "cum_died" = cum_died_spa),
       "country" = "SPA", "N" = N_spa, "birth_rate" = birth_rate_spa,
       "mort_rate" = mort_rate_spa, "exp_life" = exp_life_spa)      
