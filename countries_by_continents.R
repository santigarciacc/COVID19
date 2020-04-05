
##
## RData with countries classified by continents
##

asia <- c("AFG", "AZE", "BHR", "BGD", "BTN", "BRN", "KHM", "CHN", "GUM", "IND",
          "IDN", "IRN", "IRQ", "ISR", "JAP", "JOR", "KAZ", "KWT", "KGZ", "LAO",
          "MNG", "MYS", "MMR", "NPL", "SGP", "KOR", "UZB", "VNM", "ARE",
          "TWN", "LKA", "THA", "OMN", "PAK", "PSE", "PHL", "QAT", "SAU",
          "TLS", "TUR") # turkey is included  

africa <- c("DZA", "AGO", "BEN", "BWA", "CMR", "BDI", "BFA", "CPV", "CAF",
            "TCD", "EGY", "COG", "CIV", "COD", "DJI", "GIN", "GNB", "GNQ",
            "ERI", "SWZ", "ETH", "GAB", "GMB", "GHA", "SDN", "TGO", "TZA",
            "ZMB", "ZWE", "UGA", "TUN", "SYR", "SOM", "ZAF", "SLE", "SNE",
            "KEN", "LBN", "LBR", "LBY", "MDG", "MLI", "MRT", "MAR", "MOZ",
            "NER", "NGA", "NAM", "RWA", "MUS", "MWI", "SYC")

europe <- c("ALB", "AND", "ARM", "AUS", "BLR", "BEL", "BGR", "HRV", "CYP",
            "CZE", "DNK", "EST", "FIN", "FRA", "FRO", "GEO", "DEU", "GIB",
            "GRC", "GRL", "HUN", "ITA", "IRL", "ISL", "GGY", "VAT", "IMN",
            "JEY", "XKX", "LVA", "LIE", "LTU", "LUX", "MLT", "MDA", "MCO",
            "MNE", "NLD", "NOR", "POL", "PRT", "SRB", "SMR", "ROU", "SVK",
            "SVN", "ESP", "SWE", "CHE", "UKR", "GBR",
            "MKD", "CHE", "RUS") # armenia and rusia are included

central_north_america <- c("ATG", "ABW", "BHS", "BRB", "BEL", "BMU", "VGB",
                           "CAN", "CYM", "CRI", "CUB", "CUW", "DMA", "DOM",
                           "SLV", "GRD", "GTM", "HTI", "HND", "JAM", "MEX",
                           "MSR", "NIC", "PAN", "VIR", "TTO", "PRI", "KNA", 
                           "LCA", "VCT", "TCA", "SXM") # aruba (netherlands)

south_america <- c("ARG", "BOL", "BRA", "CHL", "COL", "ECU", "GUY", "MDV",
                   "SUN", "USA", "URY", "VEN", "PRY", "PER")

oceania <- c("AUS", "FJI", "PYF", "NCL", "NZL", "MNP", "PNG")
 
countries_by_cont <- list("europe" = europe, "asia" = asia, "africa" = africa,
                          "oceania" = oceania, "south_america" = south_america,
                          "central_north_america" = central_north_america) 

# Saving in RData
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
save(countries_by_cont, file = "countries_by_cont.RData")
  
                 


                          
                           
