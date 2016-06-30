# Functions to plot rlevels and other indices that deoends on return periods. For 1 station or a group of stations

plot4server_gof.rlevels  <- function(station, gof, r.period, quantile) { 
  
  print(gof)
  if (gof == "BS") {
    r.period.index <- which(rperiods.bs == r.period)  
  } else {r.period.index <- which(return.periods == r.period)}
  
  par(mfrow = c(3, 3), oma=c(0, 0, 0, 0), cex = 1.2)
  par(mar = c(4, 4, 2, 0))  # c(bottom, left, top, right)
  ## First subplot
  # d <- 1  # gumbel
  for (d in seq(along = distr.name)) {
    
    temp2plot <- array(NA, dim = c(3, length(sampling_years)))
    distr <- distr.name[d]
    
    for (m in 1:3) {
      method <- method.name[m] 
      temp2plot[m, ] <- var.get.nc(gof_nc, gof, start = c(station, d, m, 1, quantile, r.period.index),
                                   count = c(1, 1, 1, length(sampling_years), 1, 1)) 
    }
    if (all(is.na(temp2plot)) == FALSE) {
      
      ymin <- min(temp2plot, na.rm = TRUE) * 0.9
      ymax <- max(temp2plot, na.rm = TRUE) * 1.1
      xmax <- length(na.omit(temp2plot[1, ]))   
      
      plot(temp2plot[1, 1:length(sampling_years)], type = "l", lwd = 3, xaxt='n',
           xlim = c(1, xmax), ylim = c(ymin, ymax), col = "black",  ylab = "", xlab = "")
      par(new =TRUE)
      plot(temp2plot[2, 1:length(sampling_years)], type = "l", lwd = 3, xaxt='n',
           xlim = c(1, xmax), ylim = c(ymin, ymax), col = "red",  ylab = "", xlab = "")
      par(new =TRUE)
      if (d == 1 || d == 4) {
        plot(temp2plot[3, 1:length(sampling_years)], type = "l", lwd = 3, xaxt='n', 
             xlim = c(1, xmax), ylim = c(ymin, ymax), col = "blue",
             ylab = gof, xlab = "", main = paste(gof,"for", distr, sep = " "))
      } else {
        plot(temp2plot[3, 1:length(sampling_years)], type = "l", lwd = 3, xaxt='n', 
             xlim = c(1, xmax), ylim = c(ymin, ymax), col = "blue",
             ylab = "", xlab = "", main = paste(gof,"for", distr, sep = " ")) 
      }
      
      legend("topright", inset = .05, c("Maximum likelihood","L-moments", "Ordinary moments" ), cex = 1.2, bty = "n",
             col = c("black", "red","blue"),lty = c(1, 1, 1),lwd=c(3, 3, 3), merge = TRUE, bg = "gray90") 
      tix <- sampling_years
      axis(1, at = c(1:length(sampling_years)), labels = tix)
    } else {
      frame()
      frame()
      frame()
      frame()
      frame()
    }
  }
  frame()   # We fill the 6th plot with a blank plot
  #   legend("topleft", inset = .05, c("Maximum likelihood","L-moments", "Ordinary moments" ), cex = 1.2, bty = "n",
  #          col = c("black", "red","blue"),lty = c(1, 1, 1),lwd=c(3, 3, 3), merge = TRUE, bg = "gray90") 
  legend("center", inset = .05, distr.name, cex = 1.2, bty = "n",
         col = c("black", "red", "orange", "green", "blue"),lty = c(1, 1, 1),lwd=c(3, 3, 3), merge = TRUE, bg = "gray90") 
  
  
  ## Now we vary distr in each plot
  for (m in 1:3) {
    method <- method.name[m] 
    temp2plot <- array(NA, dim = c(5, length(sampling_years)))
    for (d in seq(along = distr.name)) {
      
      distr <- distr.name[d]
      temp2plot[d, ] <- var.get.nc(gof_nc, gof, start = c(station, d, m, 1, quantile, r.period.index),
                                   count = c(1, 1, 1, length(sampling_years), 1, 1)) 
      
    }
    if (all(is.na(temp2plot)) == FALSE) {
      ymin <- min(temp2plot, na.rm = TRUE) * 0.9
      ymax <- max(temp2plot, na.rm = TRUE) * 1.1
      xmax <- length(na.omit(temp2plot[1, ]))    
      
      plot(temp2plot[1, 1:length(sampling_years)], type = "l", lwd = 3, xaxt='n',
           xlim = c(1, xmax), ylim = c(ymin, ymax), col = "black",  ylab = "", xlab = "")
      par(new =TRUE)
      plot(temp2plot[2, 1:length(sampling_years)], type = "l", lwd = 3, xaxt='n', 
           xlim = c(1, xmax), ylim = c(ymin, ymax), col = "red",  ylab = "", xlab = "")
      par(new =TRUE)
      plot(temp2plot[3, 1:length(sampling_years)], type = "l", lwd = 3, xaxt='n', 
           xlim = c(1, xmax), ylim = c(ymin, ymax), col = "orange",  ylab = "", xlab = "")
      par(new =TRUE)
      plot(temp2plot[4, 1:length(sampling_years)], type = "l", lwd = 3, xaxt='n', 
           xlim = c(1, xmax), ylim = c(ymin, ymax), col = "green",  ylab = "", xlab = "")
      par(new =TRUE)
      if (m ==1) {
        plot(temp2plot[5, 1:length(sampling_years)], type = "l", lwd = 3, xaxt='n', 
             xlim = c(1, xmax), ylim = c(ymin, ymax), col = "blue",
             ylab = gof, xlab = "Length of record", main = paste(gof,"for", method, sep = " "))
      } else {
        plot(temp2plot[5, 1:length(sampling_years)], type = "l", lwd = 3, xaxt='n', 
             xlim = c(1, xmax), ylim = c(ymin, ymax), col = "blue",
             ylab = "", xlab = "Length of record", main = paste(gof,"for", method, sep = " "))
      }
      #    legend("topright", inset = .05, distr.name, cex = 1.2, bty = "n",
      #              col = c("black", "red", "orange", "green", "blue"),lty = c(1, 1, 1),lwd=c(3, 3, 3), merge = TRUE, bg = "gray90")  
      tix <- sampling_years
      axis(1, at = c(1:length(sampling_years)), labels = tix)
    } else {
      frame()
      frame()
      frame()
    }
  }
}

# NT is plotted differently than the other r.levels indices because it doens't have the extra "few_quantiles" dimension
plot4server_nt  <- function(station, gof, r.period) { 
  
  r.period.index <- which(rperiods.bs == r.period)  
  
  
  par(mfrow = c(3, 3), oma=c(0, 0, 0, 0), cex = 1.2)
  par(mar = c(4, 4, 2, 0))  # c(bottom, left, top, right)
  ## First subplot
  # d <- 1  # gumbel
  for (d in seq(along = distr.name)) {
    
    temp2plot <- array(NA, dim = c(3, length(sampling_years)))
    distr <- distr.name[d]
    
    for (m in 1:3) {
      method <- method.name[m] 
      temp2plot[m, ] <- var.get.nc(gof_nc, gof, start = c(station, d, m, 1, 1, r.period.index),
                                   count = c(1, 1, 1, length(sampling_years), 1)) 
    }
    if (all(is.na(temp2plot)) == FALSE) {
      
      ymin <- min(temp2plot, na.rm = TRUE) * 0.9
      ymax <- max(temp2plot, na.rm = TRUE) * 1.1
      xmax <- length(na.omit(temp2plot[1, ]))   
      
      plot(temp2plot[1, 1:length(sampling_years)], type = "l", lwd = 3, xaxt='n',
           xlim = c(1, xmax), ylim = c(ymin, ymax), col = "black",  ylab = "", xlab = "")
      par(new =TRUE)
      plot(temp2plot[2, 1:length(sampling_years)], type = "l", lwd = 3, xaxt='n',
           xlim = c(1, xmax), ylim = c(ymin, ymax), col = "red",  ylab = "", xlab = "")
      par(new =TRUE)
      if (d == 1 || d == 4) {
        plot(temp2plot[3, 1:length(sampling_years)], type = "l", lwd = 3, xaxt='n', 
             xlim = c(1, xmax), ylim = c(ymin, ymax), col = "blue",
             ylab = gof, xlab = "", main = paste(gof,"for", distr, sep = " "))
      } else {
        plot(temp2plot[3, 1:length(sampling_years)], type = "l", lwd = 3, xaxt='n', 
             xlim = c(1, xmax), ylim = c(ymin, ymax), col = "blue",
             ylab = "", xlab = "", main = paste(gof,"for", distr, sep = " ")) 
      }
      
      legend("topright", inset = .05, c("Maximum likelihood","L-moments", "Ordinary moments" ), cex = 1.2, bty = "n",
             col = c("black", "red","blue"),lty = c(1, 1, 1),lwd=c(3, 3, 3), merge = TRUE, bg = "gray90") 
      tix <- sampling_years
      axis(1, at = c(1:length(sampling_years)), labels = tix)
    } else {
      frame()
      frame()
      frame()
      frame()
      frame()
    }
  }
  frame()   # We fill the 6th plot with a blank plot
  #   legend("topleft", inset = .05, c("Maximum likelihood","L-moments", "Ordinary moments" ), cex = 1.2, bty = "n",
  #          col = c("black", "red","blue"),lty = c(1, 1, 1),lwd=c(3, 3, 3), merge = TRUE, bg = "gray90") 
  legend("center", inset = .05, distr.name, cex = 1.2, bty = "n",
         col = c("black", "red", "orange", "green", "blue"),lty = c(1, 1, 1),lwd=c(3, 3, 3), merge = TRUE, bg = "gray90") 
  
  
  ## Now we vary distr in each plot
  for (m in 1:3) {
    method <- method.name[m] 
    temp2plot <- array(NA, dim = c(5, length(sampling_years)))
    for (d in seq(along = distr.name)) {
      
      distr <- distr.name[d]
      temp2plot[d, ] <- var.get.nc(gof_nc, gof, start = c(station, d, m, 1, 6, r.period.index),
                                   count = c(1, 1, 1, length(sampling_years), 1)) 
      
    }
    if (all(is.na(temp2plot)) == FALSE) {
      ymin <- min(temp2plot, na.rm = TRUE) * 0.9
      ymax <- max(temp2plot, na.rm = TRUE) * 1.1
      xmax <- length(na.omit(temp2plot[1, ]))    
      
      plot(temp2plot[1, 1:length(sampling_years)], type = "l", lwd = 3, xaxt='n',
           xlim = c(1, xmax), ylim = c(ymin, ymax), col = "black",  ylab = "", xlab = "")
      par(new =TRUE)
      plot(temp2plot[2, 1:length(sampling_years)], type = "l", lwd = 3, xaxt='n', 
           xlim = c(1, xmax), ylim = c(ymin, ymax), col = "red",  ylab = "", xlab = "")
      par(new =TRUE)
      plot(temp2plot[3, 1:length(sampling_years)], type = "l", lwd = 3, xaxt='n', 
           xlim = c(1, xmax), ylim = c(ymin, ymax), col = "orange",  ylab = "", xlab = "")
      par(new =TRUE)
      plot(temp2plot[4, 1:length(sampling_years)], type = "l", lwd = 3, xaxt='n', 
           xlim = c(1, xmax), ylim = c(ymin, ymax), col = "green",  ylab = "", xlab = "")
      par(new =TRUE)
      if (m ==1) {
        plot(temp2plot[5, 1:length(sampling_years)], type = "l", lwd = 3, xaxt='n', 
             xlim = c(1, xmax), ylim = c(ymin, ymax), col = "blue",
             ylab = gof, xlab = "Length of record", main = paste(gof,"for", method, sep = " "))
      } else {
        plot(temp2plot[5, 1:length(sampling_years)], type = "l", lwd = 3, xaxt='n', 
             xlim = c(1, xmax), ylim = c(ymin, ymax), col = "blue",
             ylab = "", xlab = "Length of record", main = paste(gof,"for", method, sep = " "))
      }
      #    legend("topright", inset = .05, distr.name, cex = 1.2, bty = "n",
      #              col = c("black", "red", "orange", "green", "blue"),lty = c(1, 1, 1),lwd=c(3, 3, 3), merge = TRUE, bg = "gray90")  
      tix <- sampling_years
      axis(1, at = c(1:length(sampling_years)), labels = tix)
    } else {
      frame()
      frame()
      frame()
    }
  }
}



###################### Coefficient of variation

plot4server_rlevels_coeffvar  <- function(station, gof, r.period) { 
  
  print(gof)
  print(station)
  print(r.period)
  maxindex2plot <- length(sampling_years)
  
  
  if (gof == "BS") {
    r.period.index <- which(rperiods.bs == r.period)  
  } else {r.period.index <- which(return.periods == r.period)}
  
  par(mfrow = c(3, 3), oma=c(0, 0, 0, 0), cex = 1.2)
  par(mar = c(4, 4, 2, 0))  # c(bottom, left, top, right)
  ## First subplot
  # d <- 1  # gumbel
  for (d in seq(along = distr.name)) {
    
    temp2plot <- array(NA, dim = c(3, length(sampling_years)))
    distr <- distr.name[d]
    
    for (m in 1:3) {
      method <- method.name[m] 
      # coefficient of variation = st_dev / mean
      temp2plot[m, ] <- var.get.nc(gof_nc, gof, start = c(station, d, m, 1, 1, r.period.index),
                                   count = c(1, 1, 1, length(sampling_years), 1, 1)) /
        var.get.nc(gof_nc, gof, start = c(station, d, m, 1, 1, r.period.index),
                   count = c(1, 1, 1, length(sampling_years), 1, 1))
    }
    if (all(is.na(temp2plot)) == FALSE) {
      
      ymin <- min(temp2plot, na.rm = TRUE) * 0.9
      ymax <- max(temp2plot, na.rm = TRUE) * 1.1
      xmax <- length(na.omit(temp2plot[1, ]))   
      
      plot(temp2plot[1, 1:maxindex2plot], type = "l", lwd = 3, xaxt='n',
           xlim = c(1, xmax), ylim = c(ymin, ymax), col = "black",  ylab = "", xlab = "")
      par(new =TRUE)
      plot(temp2plot[2, 1:maxindex2plot], type = "l", lwd = 3, xaxt='n',
           xlim = c(1, xmax), ylim = c(ymin, ymax), col = "red",  ylab = "", xlab = "")
      par(new =TRUE)
      if (d == 1 || d == 4) {
        plot(temp2plot[3, 1:maxindex2plot], type = "l", lwd = 3, xaxt='n', 
             xlim = c(1, xmax), ylim = c(ymin, ymax), col = "blue",
             ylab = gof, xlab = "", main = paste(gof,"for", distr, sep = " "))
      } else {
        plot(temp2plot[3, 1:maxindex2plot], type = "l", lwd = 3, xaxt='n', 
             xlim = c(1, xmax), ylim = c(ymin, ymax), col = "blue",
             ylab = "", xlab = "", main = paste(gof,"for", distr, sep = " ")) 
      }
      
      legend("topright", inset = .05, c("Maximum likelihood","L-moments", "Ordinary moments" ), cex = 1.2, bty = "n",
             col = c("black", "red","blue"),lty = c(1, 1, 1),lwd=c(3, 3, 3), merge = TRUE, bg = "gray90") 
      tix <- sampling_years[1:maxindex2plot]
      axis(1, at = c(1:maxindex2plot), labels = tix)
    } else {
      frame()
      frame()
      frame()
      frame()
      frame()
    }
  }
  frame()   # We fill the 6th plot with a blank plot
  legend("center", inset = .05, distr.name, cex = 1.2, bty = "n",
         col = c("black", "red", "orange", "green", "blue"),lty = c(1, 1, 1),lwd=c(3, 3, 3), merge = TRUE, bg = "gray90") 
  
  
  ## Now we vary distr in each plot
  for (m in 1:3) {
    method <- method.name[m] 
    temp2plot <- array(NA, dim = c(5, length(sampling_years)))
    for (d in seq(along = distr.name)) {
      
      distr <- distr.name[d]
      # again coefficient of variation
      temp2plot[d, ] <- var.get.nc(gof_nc, gof, start = c(station, d, m, 1, 1, r.period.index),
                                   count = c(1, 1, 1, length(sampling_years), 1,1)) /
        var.get.nc(gof_nc, gof, start = c(station, d, m, 1, 1, r.period.index), 
                   count = c(1, 1, 1, length(sampling_years), 1,1))
      
    }
    if (all(is.na(temp2plot)) == FALSE) {
      ymin <- min(temp2plot, na.rm = TRUE) * 0.9
      ymax <- max(temp2plot, na.rm = TRUE) * 1.1
      xmax <- length(na.omit(temp2plot[1, ]))    
      
      plot(temp2plot[1, 1:maxindex2plot], type = "l", lwd = 3, xaxt='n',
           xlim = c(1, xmax), ylim = c(ymin, ymax), col = "black",  ylab = "", xlab = "")
      par(new =TRUE)
      plot(temp2plot[2, 1:maxindex2plot], type = "l", lwd = 3, xaxt='n', 
           xlim = c(1, xmax), ylim = c(ymin, ymax), col = "red",  ylab = "", xlab = "")
      par(new =TRUE)
      plot(temp2plot[3, 1:maxindex2plot], type = "l", lwd = 3, xaxt='n', 
           xlim = c(1, xmax), ylim = c(ymin, ymax), col = "orange",  ylab = "", xlab = "")
      par(new =TRUE)
      plot(temp2plot[4, 1:maxindex2plot], type = "l", lwd = 3, xaxt='n', 
           xlim = c(1, xmax), ylim = c(ymin, ymax), col = "green",  ylab = "", xlab = "")
      par(new =TRUE)
      if (m ==1) {
        plot(temp2plot[5, 1:maxindex2plot], type = "l", lwd = 3, xaxt='n', 
             xlim = c(1, xmax), ylim = c(ymin, ymax), col = "blue",
             ylab = gof, xlab = "Length of record", main = paste(gof,"for", method, sep = " "))
      } else {
        plot(temp2plot[5, 1:maxindex2plot], type = "l", lwd = 3, xaxt='n', 
             xlim = c(1, xmax), ylim = c(ymin, ymax), col = "blue",
             ylab = "", xlab = "Length of record", main = paste(gof,"for", method, sep = " "))
      }
      #    legend("topright", inset = .05, distr.name, cex = 1.2, bty = "n",
      #              col = c("black", "red", "orange", "green", "blue"),lty = c(1, 1, 1),lwd=c(3, 3, 3), merge = TRUE, bg = "gray90")  
      tix <- sampling_years[1:maxindex2plot]
      axis(1, at = c(1:maxindex2plot), labels = tix)
    } else {
      frame()
      frame()
      frame()
    }
  }
}


plot4server_rlevels_coeffvar_ave  <- function(gof, r.period, min_years, max_years) { 
  
  
  stations2ave <- stations2average(min_years, max_years)
  maxindex2plot <- 15
  
  print(gof)
  print(r.period)
  if (gof == "BS" || gof == "QS") {
    r.period.index <- which(rperiods.bs == r.period)  
  } else {r.period.index <- which(return.periods == r.period)}
  
  par(mfrow = c(3, 3), oma=c(0, 0, 0, 0), cex = 1.2)
  par(mar = c(4, 4, 2, 0))  # c(bottom, left, top, right)
  ## First subplot
  # d <- 1  # gumbel
  
  
  
  for (d in seq(along = distr.name)) {
    
    temp2plot <- array(NA, dim = c(3, length(sampling_years)))
    temp2plot_raw <- array(NA, dim = c(length(stations2ave), 3, length(sampling_years)))
    distr <- distr.name[d]
    
    for (m in 1:3) {  # TO FIX for Bayes
      method <- method.name[m] 
      
      for (st in seq(along = stations2ave)) {
        
        if (gof == "NT") {
          temp2plot_raw[st, m, ] <- var.get.nc(gof_nc, gof, start = c(station$index[stations2ave[st]], d, m, 1, r.period.index), 
                                               count = c(1, 1, 1, length(sampling_years), 1))
          
          
        } else {
          # coefficient of variation = st_dev / mean
          temp2plot_raw[st, m, ] <- var.get.nc(gof_nc, gof, start = c(station$index[stations2ave[st]], d, m, 1, 2, r.period.index), 
                                               count = c(1, 1, 1, length(sampling_years), 1, 1)) /
            var.get.nc(gof_nc, gof, start = c(station$index[stations2ave[st]], d, m, 1, 1, r.period.index), 
                       count = c(1, 1, 1, length(sampling_years), 1, 1))
          
        }
        
        
      }
      temp2plot[m, ] <- colMeans(temp2plot_raw[, m, ], na.rm = TRUE) 
    }
    
    
    
    if (all(is.na(temp2plot)) == FALSE) {
      
      ymin <- min(temp2plot, na.rm = TRUE) * 0.9
      ymax <- max(temp2plot, na.rm = TRUE) * 1.1
      xmax <- maxindex2plot   
      
      plot(temp2plot[1, 1:maxindex2plot], type = "l", lwd = 3, xaxt='n',
           xlim = c(1, xmax), ylim = c(ymin, ymax), col = "black",  ylab = "", xlab = "")
      par(new =TRUE)
      plot(temp2plot[2, 1:maxindex2plot], type = "l", lwd = 3, xaxt='n',
           xlim = c(1, xmax), ylim = c(ymin, ymax), col = "red",  ylab = "", xlab = "")
      par(new =TRUE)
      if (d == 1 || d == 4) {
        plot(temp2plot[3, 1:maxindex2plot], type = "l", lwd = 3, xaxt='n', 
             xlim = c(1, xmax), ylim = c(ymin, ymax), col = "blue",
             ylab = gof, xlab = "", main = paste(gof,"for", distr, sep = " "))
      } else {
        plot(temp2plot[3, 1:maxindex2plot], type = "l", lwd = 3, xaxt='n', 
             xlim = c(1, xmax), ylim = c(ymin, ymax), col = "blue",
             ylab = "", xlab = "", main = paste(gof,"for", distr, sep = " ")) 
      }
      
      legend("topright", inset = .05, c("Maximum likelihood","L-moments", "Ordinary moments" ), cex = 1.2, bty = "n",
             col = c("black", "red","blue"),lty = c(1, 1, 1),lwd=c(3, 3, 3), merge = TRUE, bg = "gray90") 
      tix <- sampling_years[1:maxindex2plot]
      axis(1, at = c(1:maxindex2plot), labels = tix)
    } else {
      frame()
      frame()
      frame()
      frame()
      frame()
    }
  }
  frame()   # We fill the 6th plot with a blank plot
  #   legend("topleft", inset = .05, c("Maximum likelihood","L-moments", "Ordinary moments" ), cex = 1.2, bty = "n",
  #          col = c("black", "red","blue"),lty = c(1, 1, 1),lwd=c(3, 3, 3), merge = TRUE, bg = "gray90") 
  legend("center", inset = .05, distr.name, cex = 1.2, bty = "n",
         col = c("black", "red", "orange", "green", "blue"),lty = c(1, 1, 1),lwd=c(3, 3, 3), merge = TRUE, bg = "gray90") 
  
  
  ## Now we vary distr in each plot
  for (m in 1:3) {
    method <- method.name[m] 
    temp2plot <- array(NA, dim = c(5, length(sampling_years)))
    temp2plot_raw <- array(NA, dim = c(length(stations2ave), 5, length(sampling_years)))
    for (d in seq(along = distr.name)) {
      
      distr <- distr.name[d]
      # again coefficient of variation
      for (st in seq(along = stations2ave)) {
        
        if (gof == "NT") {
          
          temp2plot_raw[st, d, ] <- var.get.nc(gof_nc, gof, start = c(station$index[stations2ave[st]], d, m, 1, r.period.index), 
                                               count = c(1, 1, 1, length(sampling_years), 1)) 
          
        } else {
          temp2plot_raw[st, d, ] <- var.get.nc(gof_nc, gof, start = c(station$index[stations2ave[st]], d, m, 1, 2, r.period.index), 
                                               count = c(1, 1, 1, length(sampling_years), 1, 1)) /
            var.get.nc(gof_nc, gof, start = c(station$index[stations2ave[st]], d, m, 1, 1, r.period.index), 
                       count = c(1, 1, 1, length(sampling_years), 1, 1))     
          
        }
        
        
      }
      temp2plot[d, ] <- colMeans(temp2plot_raw[, d, ], na.rm = TRUE) 
    }
    
    if (all(is.na(temp2plot)) == FALSE) {
      ymin <- min(temp2plot, na.rm = TRUE) * 0.9
      ymax <- max(temp2plot, na.rm = TRUE) * 1.1
      xmax <- maxindex2plot    
      
      plot(temp2plot[1, 1:maxindex2plot], type = "l", lwd = 3, xaxt='n',
           xlim = c(1, xmax), ylim = c(ymin, ymax), col = "black",  ylab = "", xlab = "")
      par(new =TRUE)
      plot(temp2plot[2, 1:maxindex2plot], type = "l", lwd = 3, xaxt='n', 
           xlim = c(1, xmax), ylim = c(ymin, ymax), col = "red",  ylab = "", xlab = "")
      par(new =TRUE)
      plot(temp2plot[3, 1:maxindex2plot], type = "l", lwd = 3, xaxt='n', 
           xlim = c(1, xmax), ylim = c(ymin, ymax), col = "orange",  ylab = "", xlab = "")
      par(new =TRUE)
      plot(temp2plot[4, 1:maxindex2plot], type = "l", lwd = 3, xaxt='n', 
           xlim = c(1, xmax), ylim = c(ymin, ymax), col = "green",  ylab = "", xlab = "")
      par(new =TRUE)
      if (m ==1) {
        plot(temp2plot[5, 1:maxindex2plot], type = "l", lwd = 3, xaxt='n', 
             xlim = c(1, xmax), ylim = c(ymin, ymax), col = "blue",
             ylab = gof, xlab = "Length of record", main = paste(gof,"for", method, sep = " "))
      } else {
        plot(temp2plot[5, 1:maxindex2plot], type = "l", lwd = 3, xaxt='n', 
             xlim = c(1, xmax), ylim = c(ymin, ymax), col = "blue",
             ylab = "", xlab = "Length of record", main = paste(gof,"for", method, sep = " "))
      }
      tix <- sampling_years[1:maxindex2plot]
      axis(1, at = c(1:maxindex2plot), labels = tix)
    } else {
      frame()
      frame()
      frame()
    }
  }
}
