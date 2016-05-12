## Supporting functions for Shiny App: functions that pre-process data for the plotting, mapping or table functions

gof_summary <- function(gof, station) {
  
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
  invisible(results)  
}

gof_summary_rperiods <- function(gof, station, r.period) {
  
  if (gof == "BS" || gof == "NT") {
    r.period.index <- which(rperiods.bs == r.period)  
  } else {r.period.index <- which(return.periods == r.period)}
  
  gof.table <- data.frame(gum = rep(NA,4), gam = rep(NA,4), gev = rep(NA,4), glo = rep(NA,4), pe3 = rep(NA,4))
  row.names(gof.table) <- method.name
  for (m in seq(along = method.name)) {
    for (d in seq(along = distr.name)) {
      if (gof == "r.levels") {
      gof.table[m,d] <- round(var.get.nc(gof_nc, gof, start = c(station, d, m, 30, 1, r.period.index),
                                         count = c(1, 1, 1, 1, 1, 1)), 0)  # in dataframe [row, column]
      } else {
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

# Function for updating the df to put in the interactive table
group.dfmaker <- function(group.indexes) {
  
  group.df <- data.frame("Station name" = station$name[group.indexes], 
                         "NVE number" = station$nve_nb[group.indexes],
                         "Length of record" = station$length_rec[group.indexes])
  
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





