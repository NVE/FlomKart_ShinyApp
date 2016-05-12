# Functions to plot the raw input data in the first tab


# length_dat <- vector(length = length(station.nb.vect))
# 
# for (i in seq(along = station.nb.vect)) {
#   sdat <- sdat_load(dat, station.nb.vect[i])
#   length_dat[i] <- length(sdat$flom_DOGN)
#   
# }
# hist(length_dat, breaks = 20 , xlab = "Lenth of flood record", ylab = "Number of stations", main= "", cex.lab = 1.5)
# 


########################################## TEST OF BOX PLOT FOR ALL STATIONS THAT HAVE MORE THAN N data
boxplot.param <- function(min_data, max_data) {
  
  
  
  flood_list <- list()
  station_list <- c()
  for (i in 1:length(station.nb.vect)) {
    # assign(paste("sdat",i,sep = ""), sdat_load(dat, station.nb.vect[i]) )
    if (!is.null(sdat_load(dat, station.nb.vect[i]))) {
      
      flood_list[[i]] <- sdat_load(dat, station.nb.vect[i])$flom_DOGN / mean(na.omit(sdat_load(dat, station.nb.vect[i])$flom_DOGN))
      station_list[i] <- as.character(sdat_load(dat, station.nb.vect[i])$name[1])
    } else {
      flood_list[[i]] <- NA
      station_list[i] <- NA
    }
  }
  to_plot <- c()
  k <- 0
  for (i in 1:length(station.nb.vect)) {
    
    if (length(flood_list[[i]]) > min_data && length(flood_list[[i]]) < max_data) {
      k <- k+1
      to_plot[k] <- i
    }
  }
  
  par(las = 1) # all axis labels horizontal
  boxplot(flood_list[to_plot], names = station_list[to_plot], horizontal = TRUE, outwex = TRUE)
  
  max_vect <- colMaxs(flood_list[to_plot])
  
  boxplot(sort(flood_list[to_plot], decreasing = TRUE), horizontal = TRUE)
  
}