# Preloading of NetCDF files and other pre-processing that should only happen once

# rm(list = ls())  # clean out workspace and set working directorY
# setwd('C:/Users/flbk/Documents/GitHub/FlomKart_ShinyApp')  
# to consider appDir = getwd()

# Installing and loading required packages
packages <- c("shiny", "leaflet", "RNetCDF", "magrittr", "evd", "nsRFA", "formattable", "DT", "shinyBS", "plotly")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

# apply(as.list(packages),library)

library(shiny)        # TO run the app!
library(leaflet)      # For the interactive map    
library(RNetCDF)      # To read the data in netCDF files
library(magrittr)     # For piping functions
library(evd)          # Functions for extreme value distributions
library(nsRFA)        # For the GL distrib (,aybe pearson too)
library(formattable)  # To add formatting to data tables
library(DT)           # for the data tables
library(shinyBS)      # for the interactive popover features
library(plotly)       # for interactive graphs

library(FlomKartShinyApp) 

# source('R/rawdata_plotting.R')
# source('R/mapping.R')
# source('R/main_plotting.R')  # plots for the first "main plots" subtab
# source('R/gof_plotting.R')  # GOF plots
# source('R/rperiods_plotting.R')  # Plots that are a function of return periods (return levels, QS, BS, NT)


# dat <- read.csv("//nve/fil/h/HM/Interne Prosjekter/Flomkart/Model_fitting/Florian/Data/AMS_table_updated.csv", sep=";", as.is=TRUE)  
# dat$date_DOGN <- as.POSIXlt(dat$date_DOGN)
# dummy_date <- dat$date_DOGN
# dat$day <- dummy_date$mday        # day of month
# dat$month <- dummy_date$mon + 1     # month of year (zero-indexed)

# dat$year_test <- dummy_date$year+1800  # years since 1900
# for (i in seq(along = station$number) ) {
#   Q_years[i, ] <- XXX
# }

# plot(dat$date_DOGN, dat$flom_DOGN)
# 
# barplot(dat$flom_DOGN, dat$year)


nc <- open.nc("data/flood_database.nc", write = FALSE)  # Put FALSE for read-only  
gof_nc <- open.nc("data/gof.nc", write = FALSE)  # Put FALSE for read-only  

# Compare with gof_noXvalid.nc
# gof_nc <- open.nc("output/gof_noXvalid.nc", write = FALSE)  # Put FALSE for read-only


Q <- var.get.nc(nc, "Q")
distr.name <- var.get.nc(nc, "distr.name")
method.name <- var.get.nc(nc, "method.name")
return.periods <- var.get.nc(gof_nc, "r.periods")

# For the map
station <- list()
station$name <- var.get.nc(nc, "station.name")
station$number <- var.get.nc(nc, "station.number")
# station$nve_nb <- var.get.nc(nc, "station.nve_nb")
station$utmN <- var.get.nc(nc, "station.utmN")
station$utmE <- var.get.nc(nc, "station.utmE")
station$long <- var.get.nc(nc, "station.long")
station$lat <- var.get.nc(nc, "station.lat")
station$catchment.size <- var.get.nc(nc, "catchment.size")
station$catchment.min.height <- var.get.nc(nc, "catchment.min.height")
station$catchment.max.height <- var.get.nc(nc, "catchment.max.height")

# dates <- var.get.nc(nc, "dates")
years <- var.get.nc(nc, "years")


dim.random_runs <- var.get.nc(nc, "dim.random_runs")
sampling_years <<- var.get.nc(nc, "sampling_years")
sampling_years_full_record <- c(as.character(sampling_years), NA, NA, NA, NA, NA, NA, "FULL RECORD")

rperiods.bs <- c(2,5,10,15,20,30)  # Those are return periods for BS ans NT. They should be saved in the NetCDF

## How can the following be replaced by apply (# station$length_rec <- apply(na.omit(Q), length...)?)
station$length_rec <- as.vector(rep(NA,length(station$number)))

for (st in seq(along = station$number)) {
  station$length_rec[st] <- length(as.vector(na.omit(Q[st, ])))
}

keep <- which(!is.na(station$lat + station$long))
## Lets keep only the stations that have more than 30 years of data
keep <- intersect(keep, which(station$length_rec > 29))

station$index <- keep
station$length_rec <- station$length_rec[keep]
station$name <- station$name[keep]
station$number <- station$number[keep]
# station$nve_nb <- station$nve_nb[keep]
station$long <- station$long[keep]
station$lat <- station$lat[keep]
station$utmN <- station$utmN[keep]
station$utmE <- station$utmE[keep]
station$catchment.size <- station$catchment.size[keep]
station$catchment.min.height <- station$catchment.min.height[keep]
station$catchment.max.height <- station$catchment.max.height[keep]

# adding the shape param for gev (it is the 3rd parameter)
gev.shape.estimate <- var.get.nc(nc, "param.estimate", 
                                 start = c(1, 3, 1, 3, 30, 1), # from each station, distr number 3, 
                                 # method number 1 to 4, parameter number 3, full length o record, random run number 1 
                                 count = c(length(station$name), 1, 4, 1, 1, 1) ) 

# Create here the data frame that will be use in DT tables in the code
stations.summary.df <- data.frame("Station name" = station$name, 
                                  
                                  "Length of record" = station$length_rec,
                                  "Shape param GEV_Lmom" = gev.shape.estimate[ , 2],
                                  "Catchment area" = station$catchment.size,
                                  "Min elevation" = station$catchment.min.height,
                                  "Max elevation" = station$catchment.max.height
)  # taken out "NVE number" = station$nve_nb,














## Supporting functions for Shiny App: functions that pre-process data for the plotting, mapping or table functions

gof_summary <- function(gof, station) {
  
#   print(gof)
#   print(station)
  
  gof.table <- data.frame(gum = rep(NA,4), gam = rep(NA,4), gev = rep(NA,4), glo = rep(NA,4), pe3 = rep(NA,4))
  # gof.table <- matrix(NA,4,5)
  min.gof <- 100
  max.gof <- 0
  min.index <- c(NA, NA)
  max.index <- c(NA, NA)
  
  row.names(gof.table) <- method.name
  
  for (m in seq(along = method.name)) {
    for (d in seq(along = distr.name)) {
      gof.table[m,d] <- var.get.nc(gof_nc, gof, start = c(station, d, m, 30, 1),
                                   count = c(1, 1, 1, 1, 1))  # in dataframe [row, column]
#       print("print(gof.table[m,d])")
#       print(gof.table[m,d])
      
      if (!is.na(gof.table[m,d])) {
        if (gof.table[m,d] < min.gof) {
          min.gof <- gof.table[m,d]
          min.index <- c(m, d)
        }
        if (gof.table[m,d] > max.gof) {
          max.gof <- gof.table[m,d]
          max.index <- c(m, d)
        }
        
      }
      gof.table[m,d] <- round(gof.table[m,d], 4)
      
    }
  }
  
  min.gof <- round(min.gof, 4)
  
  max.gof <- round(max.gof, 4)

  
  results <- list(min_gof = min.gof, min_index = min.index, max_gof = max.gof, max_index = max.index, gof_table = gof.table)

#   print("results")
#     print(results)

  invisible(results)  
}

gof_summary_rperiods <- function(gof, station, r.period) {
  
  if (gof == "BS" || gof == "QS") {
    r.period.index <- which(rperiods.bs == r.period)  
  } else {r.period.index <- which(return.periods == r.period)}
  
  print("rperiods.bs")
  print(rperiods.bs)

  print("return.periods")
  print(return.periods)

  print("r.period")
  print(r.period)  
  
  print("r.period.index")
  print(r.period.index)  
  
  gof.table <- data.frame(gum = rep(NA,4), gam = rep(NA,4), gev = rep(NA,4), glo = rep(NA,4), pe3 = rep(NA,4))
  row.names(gof.table) <- method.name
  for (m in seq(along = method.name)) {
    for (d in seq(along = distr.name)) {
      if (gof == "r.levels") {
        print("r.period.index")
        print(r.period.index)
        r.period.index <- as.numeric(r.period.index)
        print(c(station, d, m, 30, 1, r.period.index))
      gof.table[m,d] <- round(var.get.nc(gof_nc, gof, start = c(station, d, m, 30, 1, r.period.index),
                                         count = c(1, 1, 1, 1, 1, 1)), 0)  # in dataframe [row, column]
      } else {
        
        print("r.period.index")
        print(r.period.index)
        r.period.index <- as.numeric(r.period.index)
        print(c(station, d, m, 30, 1, r.period.index))
        
      gof.table[m,d] <- round(var.get.nc(gof_nc, gof, start = c(station, d, m, 30, 1, r.period.index),
                                           count = c(1, 1, 1, 1, 1, 1)), 2)  # in dataframe [row, column] 
      }
    }
  }
  invisible(gof.table)
  
}

# Function to calculate the indexes of a group of common stations
bestgof.indexing <- function(gof, minmax) {
  
  bestgof.indexes <- matrix(NA,length(station$index),2)
  
  # Caution: station$index is required when reading the NetCDF files as we need to get back to the whole set of stations (445)
  for (st in seq(along = station$index)) {
    
    temp.list <- gof_summary(gof, station$index[st])
    if (minmax == "min") {
      bestgof.indexes[st,] <- temp.list$min_index
    }
    if (minmax == "max") {
      bestgof.indexes[st,] <- temp.list$max_index
    } 
    
    
  }
  invisible(bestgof.indexes)
}

# This function extracts from the matrix created above, the station indexes corresponding to the user's choices
station_group_indexes <- function(gof, distr, method, minmax) {
  
  group.indexes <- rep(NA,length(station$number))
  bestgof.indexes <- bestgof.indexing(gof, minmax)
  
  if (distr == "ALL" && method != "ALL") {
    m <- which(method.name == method)
    group.indexes <- intersect(seq(1,length(station$number)), which(bestgof.indexes[,1] == m))  
    
  }
  if (distr != "ALL" && method == "ALL") {
    d <- which(distr.name == distr)
    group.indexes <- intersect(seq(1,length(station$number)), which(bestgof.indexes[,2] == d))
  }
  if (distr == "ALL" && method == "ALL") {
    group.indexes <- seq(1,length(station$number))
  }
  if (distr != "ALL" && method != "ALL") {
    
    d <- which(distr.name == distr)
    m <- which(method.name == method)
    
    group.indexes <- intersect(which(bestgof.indexes[,2] == d), which(bestgof.indexes[,1] == m))  
  }
  invisible(group.indexes)
}

# This function extracts the station indexes corresponding to the user's choices for the first tab
station_group_indexes_first_tab <- function(min_years, max_years, min_height, max_height) {

  group.indexes <- rep(NA,length(station$number))
  
  group.indexes <- which(station$length_rec > min_years)
  group.indexes <- intersect(group.indexes, which(station$length_rec < max_years))
  group.indexes <- intersect(group.indexes, which(station$catchment.min.height > min_height))
  group.indexes <- intersect(group.indexes, which(station$catchment.max.height < max_height))
  
  
  
}






# Function for updating the df to put in the interactive table
group.dfmaker <- function(group.indexes) {
  
  group.df <- data.frame("Station number" = station$number[group.indexes],
                         "Length of record" = station$length_rec[group.indexes],
  "Catchment area" = station$catchment.size[group.indexes],
  "Min elevation" = station$catchment.min.height[group.indexes],
  "Max elevation" = station$catchment.max.height[group.indexes])
  
  print("group.indexes")
  
    print(group.indexes)
    print("group.df")
    
      print(group.df)
  
  invisible(group.df)
}

stations2average <- function(min_years, max_years) {
  
  stations2ave <- c()
  print("entered the station2avergae function")
  print(min_years)
  print(max_years)
  
  print(class(max_years))
  
  min_years <- as.numeric(min_years)
  max_years <- as.numeric(max_years)
  
  print(class(max_years))
  
  for (st in seq(along = station$index)) {
    if (station$length_rec[st] >= min_years) {
      if (station$length_rec[st] <= max_years) {
        stations2ave <- c(stations2ave, st)
        print("prout1")
        
      }
    }
  }
  
  invisible(stations2ave)
}





